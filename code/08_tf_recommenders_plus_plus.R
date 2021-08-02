options("reticulate.conda_binary" = "/Users/clente/miniforge3/bin/conda")

# Load R packages
library(magrittr)
library(reticulate)

# Setup datasets
# use_condaenv("tfrs", required = TRUE)
# pd <- import("pandas", convert = FALSE)
# tfds <- import("tensorflow_datasets", convert = FALSE)
# ratings <- tfds$load("movielens/1m-ratings", split = "train")
# pd$DataFrame(ratings)$to_csv("data-raw/raw-1m-ratings.csv")
# 
# "data-raw/raw-1m-ratings.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(
#     movie_id, user_id, user_rating, user_gender,
#     user_zip_code, user_occupation_text, bucketized_user_age
#   ) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b?'?") %>%
#   dplyr::mutate_all(stringr::str_remove, "'?, shape.+$") %>%
#   # dplyr::mutate(
#   #   movie_id = as.integer(movie_id),
#   #   user_id = as.integer(user_id),
#   #   user_rating = as.integer(user_rating),
#   #   user_gender = ifelse(user_gender == "Tru", 1L, 0L),
#   #   bucketized_user_age = as.integer(bucketized_user_age)
#   # ) %>%
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
  
  # # Temporary files
  # tmp_ratings <- fs::file_temp(ext = "csv")
  # on.exit({ fs::file_delete(tmp_ratings) })
  # ratings %>% 
  #   dplyr::mutate(user_gender = as.integer(user_gender)) %>% 
  #   dplyr::mutate_all(as.character) %>% 
  #   readr::write_csv(tmp_ratings, col_names = FALSE)
  # 
  # # Datasets with basic features
  # ratings <- tf$data$experimental$CsvDataset(tmp_ratings, list(
  #   tf$string, tf$string, tf$string, tf$int32,
  #   tf$string, tf$string, tf$int32
  # ))
  # 
  # ratings <- ratings$map(function(a, b, c, d, e, f, g) {
  #   list(
  #     movie_id = tf$cast(a, tf$string),
  #     user_id = tf$cast(b, tf$string),
  #     user_rating = tf$cast(c, tf$string),
  #     user_gender = tf$cast(d, tf$int32),
  #     user_zip_code = tf$cast(e, tf$string),
  #     user_occupation_text = tf$cast(f, tf$string),
  #     bucketized_user_age = tf$cast(g, tf$int32)
  #   )
  # })
  
  ratings <- tfds$load("movie_lens/100k-ratings", split = "train")
  ratings <- ratings$map(function(x) {
    list(
      movie_id = x["movie_id"],
      user_id = x["user_id"],
      user_rating = x["user_rating"],
      user_gender = tf$cast(x["user_gender"], tf$int32),
      user_zip_code = x["user_zip_code"],
      user_occupation_text = x["user_occupation_text"],
      bucketized_user_age = tf$cast(x["bucketized_user_age"], tf$int32)
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
      
      self$`_deep_layers` <- purrr::map(deep_layer_sizes, ~tf$keras$layers$Dense(.x, activation = "relu"))
      
      self$`_logit_layer` <- tf$keras$layers$Dense(1L)
      
      self$task <- tfrs$tasks$Ranking(
        loss = tf$keras$losses$MeanSquaredError(),
        metrics = c(tf$keras$metrics$RootMeanSquaredError("RMSE"))
      )
      
      return(NULL)
    },
    
    call = function(self, features, training = NULL, mask = NULL) {
      embeddings <- list()
      
      message("teste")
      for (i in seq_along(self$`_all_features`) ) {
        feature_name <- self$`_all_features`[[i-1]]
        embedding_fn <- self$`_embeddings`[[feature_name]]
        message(feature_name)
        message(self$`_embeddings`[[feature_name]])
        message(embedding_fn(features[[feature_name]]))
        embeddings <- c(embeddings, embedding_fn(features[[feature_name]]))
      }
      message("teste2")
      message(embeddings)
      
      x <- tf$concat(embeddings, axis = 1L)
      message("teste3")
      
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
      
      message("help")
      labels <- features[["user_rating"]]
      features <- features[names(features) != "user_rating"]
      message("help2")
      
      message("help3")
      scores <- self$call(features)
      message("help4")
      
      return(self$task(labels=labels, predictions=scores))
    }
    
  ), inherit = tfrs$models$Model)
  
  cached_train <- train$shuffle(100000L)$batch(8192L)$cache()
  
  epochs = 8L
  learning_rate = 0.01
  use_cross_layer = TRUE
  deep_layer_sizes = c(192L, 192L)
  projection_dim = NULL

  model <- DCN(use_cross_layer = use_cross_layer, deep_layer_sizes = deep_layer_sizes, projection_dim = projection_dim)
  model$compile(optimizer = tf$keras$optimizers$Adam(learning_rate))
  
  # cached_train <- train$map(function(x) {
  #   list(
  #     movie_id = tf$cast(x["movie_id"], tf$string),
  #     user_id = tf$cast(x["user_id"], tf$string),
  #     user_rating = tf$cast(x["user_rating"], tf$string),
  #     user_gender = tf$cast(x["user_gender"], tf$int32),
  #     user_zip_code = tf$cast(x["user_zip_code"], tf$string),
  #     user_occupation_text = tf$cast(x["user_occupation_text"], tf$string),
  #     bucketized_user_age = tf$cast(x["bucketized_user_age"], tf$int32)
  #   )
  # })
  
  model$fit(cached_train, epochs = epochs, verbose = FALSE)
  
  return(model)
}

# Get recommendations
get_recs <- function(id, index) {
  id <- as.character(id)
  titles <- py_to_r(index$call(np$array(list(id)))[[1]][0]$numpy()$tolist())
  purrr::map_chr(titles, ~.x$decode("utf-8"))
}

ratings <- readr::read_csv("data-raw/1m-ratings.csv")
model <- train_model(ratings)

model$predict(ratings$batch(128L))












