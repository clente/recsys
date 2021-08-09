options("reticulate.conda_binary" = "/Users/clente/miniforge3/bin/conda")

# Load R packages
library(magrittr)
library(reticulate)

# Setup datasets
# use_condaenv("tfrs", required = TRUE)
# pd <- import("pandas", convert = FALSE)
# tfds <- import("tensorflow_datasets", convert = FALSE)
# ratings <- tfds$load("movielens/25m-ratings", split = "train")
# pd$DataFrame(ratings)$to_csv("data-raw/raw-25m-ratings.csv")
# 
# "data-raw/raw-1m-ratings.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(
#     movie_id, user_id, user_rating, user_gender,
#     user_zip_code, user_occupation_text, bucketized_user_age
#   ) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b?'?") %>%
#   dplyr::mutate_all(stringr::str_remove, "'?, shape.+$") %>%
#   dplyr::mutate(
#     movie_id = as.integer(movie_id),
#     user_id = as.integer(user_id),
#     user_rating = as.integer(user_rating),
#     user_gender = ifelse(user_gender == "Tru", 1L, 0L),
#     bucketized_user_age = as.integer(bucketized_user_age)
#   ) %>%
#   readr::write_csv("data-raw/1m-ratings.csv")

# Load env
use_condaenv("tfrs", required = TRUE)

# Modules
bi <- reticulate::import_builtins(convert = FALSE)
np <- import("numpy", convert = FALSE)
tf <- import("tensorflow", convert = FALSE)
tfds <- import("tensorflow_datasets", convert = FALSE)
tfrs <- import("tensorflow_recommenders", convert = FALSE)

train_model <- function(ratings) {
  
  # Temporary files
  tmp_ratings <- fs::file_temp(ext = "csv")
  on.exit({ fs::file_delete(tmp_ratings) })
  ratings %>%
    readr::write_csv(tmp_ratings, col_names = FALSE)
  
  # Datasets with basic features
  ratings <- tf$data$experimental$CsvDataset(tmp_ratings, list(
    tf$string, tf$string, tf$int32, tf$int32,
    tf$string, tf$string, tf$int32
  ), header = FALSE)
  
  # Add names to columns
  ratings <- ratings$map(function(a, b, c, d, e, f, g) {
    list(
      movie_id = a,
      user_id = b,
      user_rating = c,
      user_gender = d,
      user_zip_code = e,
      user_occupation_text = f,
      bucketized_user_age = g
    )
  })
  
  tf$random$set_seed(42L)
  shuffled <- ratings$shuffle(100000L, seed = 42L, reshuffle_each_iteration = FALSE)
  
  train = shuffled$take(80000L)
  test = shuffled$skip(80000L)$take(20000L)
  
  feature_names = c("movie_id", "user_id", "user_zip_code", "user_gender",
                    "user_occupation_text", "bucketized_user_age")
  
  vocabularies = list()
  
  for (feature_name in feature_names) {
    vocab <- ratings$batch(1000000L)$map(function(x) { x[feature_name] })
    vocabularies[[feature_name]] <- np$unique(np$concatenate(bi$list(vocab)))
  }
  
  DCN <- PyClass("DCN", list(
    
    `__module__` = "workaround",
    
    `__init__` = function(self, use_cross_layer, deep_layer_sizes, projection_dim = NULL) {
      
      super()$`__init__`()
      
      self$embedding_dimension <- 32L
      
      str_features <- c("movie_id", "user_id", "user_zip_code",
                        "user_occupation_text")
      int_features <- c("user_gender", "bucketized_user_age")
      
      self$`_all_features` <- c(str_features , int_features)
      self$`_embeddings` <- list()
      
      # Compute embeddings for string features.
      for (feature_name in str_features) {
        vocabulary <- vocabularies[[feature_name]]
        self$`_embeddings`[[feature_name]] <- tf$keras$Sequential(c(
          tf$keras$layers$experimental$preprocessing$StringLookup(vocabulary = vocabulary, mask_token = NULL),
          tf$keras$layers$Embedding(length(vocabulary) + 1L, self$embedding_dimension)
        ))
      }
      
      # Compute embeddings for int features.
      for (feature_name in int_features) {
        vocabulary <- vocabularies[[feature_name]]
        self$`_embeddings`[[feature_name]] <- tf$keras$Sequential(c(
          tf$keras$layers$experimental$preprocessing$IntegerLookup(vocabulary = vocabulary, mask_token = NULL),
          tf$keras$layers$Embedding(length(vocabulary) + 1L, self$embedding_dimension)
        ))
      }
      
      if (use_cross_layer) {
        self$`_cross_layer` <- tfrs$layers$dcn$Cross(projection_dim = projection_dim, kernel_initializer = "glorot_uniform")
      } else {
        self$`_cross_layer` <- NULL
      }
      
      self$`_deep_layers` <- purrr::map(
        deep_layer_sizes, ~tf$keras$layers$Dense(.x, activation = "relu")
      )
      
      self$`_logit_layer` <- tf$keras$layers$Dense(1L)
      
      self$task <- tfrs$tasks$Ranking(
        loss = tf$keras$losses$MeanSquaredError(),
        metrics = c(tf$keras$metrics$RootMeanSquaredError("RMSE"))
      )
      
      return(NULL)
    },
    
    call = function(self, features, training = NULL, mask = NULL) {
      embeddings <- list()
      
      for (i in seq_along(self$`_all_features`) ) {
        feature_name <- self$`_all_features`[[i-1]]
        embedding_fn <- self$`_embeddings`[[feature_name]]
        embeddings <- c(embeddings, embedding_fn(features[[feature_name]]))
      }
      
      x <- tf$concat(embeddings, axis = 1L)
      
      # Build Cross Network
      if (!is.null(self$`_cross_layer`)) {
        x <- self$`_cross_layer`(x)
      }
      
      # Build Deep Network
      for (i in seq_along(self$`_deep_layers`)) {
        x <- self$`_deep_layers`[[i-1]](x)
      }
      
      return(self$`_logit_layer`(x))
    },
    
    compute_loss = function(self, features, training = FALSE) {
      
      labels <- features[["user_rating"]]
      features <- features[names(features) != "user_rating"]
      
      scores <- self$call(features)
      
      return(self$task(labels = labels, predictions = scores))
    }
    
  ), inherit = tfrs$models$Model)
  
  cached_train <- train$shuffle(100000L)$batch(8192L)$cache()
  
  epochs = 8L
  learning_rate = 0.01
  use_cross_layer = TRUE
  deep_layer_sizes = c(192L, 192L)
  projection_dim = NULL
  
  model <- DCN(
    use_cross_layer = use_cross_layer, deep_layer_sizes = deep_layer_sizes,
    projection_dim = projection_dim
  )
  model$compile(optimizer = tf$keras$optimizers$Adam(learning_rate))
  
  model$fit(cached_train, epochs = epochs, verbose = FALSE)
  
  return(model)
}

# Get recommendations
get_recs <- function(ratings, model) {
  
  # Temporary files
  tmp_ratings <- fs::file_temp(ext = "csv")
  on.exit({ fs::file_delete(tmp_ratings) })
  ratings %>%
    readr::write_csv(tmp_ratings, col_names = FALSE)
  
  # Datasets with basic features
  ratings <- tf$data$experimental$CsvDataset(tmp_ratings, list(
    tf$string, tf$string, tf$int32, tf$int32,
    tf$string, tf$string, tf$int32
  ), header = FALSE)
  
  # Add names to columns
  ratings <- ratings$map(function(a, b, c, d, e, f, g) {
    list(
      movie_id = a,
      user_id = b,
      user_rating = c,
      user_gender = d,
      user_zip_code = e,
      user_occupation_text = f,
      bucketized_user_age = g
    )
  })
  
  model$predict(ratings$batch(128L))[,1]
}

ratings <- readr::read_csv("data-raw/1m-ratings.csv")
model <- train_model(ratings)

# movies <- ratings %>% 
#   dplyr::select(movie_id) %>% 
#   dplyr::distinct() %>% 
#   dplyr::arrange(movie_id) %>% 
#   dplyr::rowwise() %>% 
#   dplyr::group_split()
# 
# users <- ratings %>% 
#   dplyr::select(-movie_id, -user_rating) %>% 
#   dplyr::distinct() %>% 
#   dplyr::arrange(user_id) %>% 
#   dplyr::rowwise() %>% 
#   dplyr::group_split()
#
# list(movie = movies, user = users) %>%
#   purrr::cross_df() %>%
#   tidyr::unnest(dplyr::everything()) %>%
#   dplyr::mutate_if(is.numeric, as.integer) %>%
#   dplyr::mutate(user_ratings = 0) %>%
#   dplyr::relocate(user_ratings, .after = user_id) %>%
#   readr::write_csv("data-raw/all_combinations.csv")

all_combinations <- readr::read_csv("data-raw/all_combinations.csv", lazy = TRUE)
amostra <- dplyr::slice_sample(all_combinations, prop = 0.01)
recs <- get_recs(amostra, model)

df <- amostra %>% 
  dplyr::mutate(y = recs) %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::slice_max(y, n = 10) %>% 
  dplyr::ungroup() %>% 
  dplyr::count(movie_id) %>% 
  dplyr::arrange(-n) %>% 
  tibble::rowid_to_column()

ggplot2::qplot(df$rowid, df$n, geom = "point")

#


