
# Load R packages
library(magrittr)
library(reticulate)

# Load env
use_condaenv("tfrs", required = TRUE)

# Modules
bi <- reticulate::import_builtins(convert = FALSE)
np <- import("numpy", convert = FALSE)
tf <- import("tensorflow", convert = FALSE)
tfds <- import("tensorflow_datasets", convert = FALSE)
tfrs <- import("tensorflow_recommenders", convert = FALSE)

ratings <- tfds$load("movielens/100k-ratings", split = "train")
# movies <- tfds$load("movie_lens/100k-movies", split = "train")

ratings_ <- ratings$map(function(x) {
  list(
    "movie_id" = x["movie_id"],
    "user_id" = x["user_id"],
    "user_rating" = x["user_rating"],
    "user_zip_code" = x["user_zip_code"],
    "user_occupation_text" = x["user_occupation_text"]
  )
})

tf$random$set_seed(42L)
shuffled <- ratings_$shuffle(100000L, seed = 42L, reshuffle_each_iteration = FALSE)

train = shuffled$take(80000L)
test = shuffled$skip(80000L)$take(20000L)

feature_names = c("movie_id", "user_id", "user_zip_code", # "user_gender"
                  "user_occupation_text") #, "bucketized_user_age")

vocabularies = list()

for (feature_name in feature_names) {
  vocab <- ratings$padded_batch(1000000L)$map(function(x) { x[feature_name]
    # if (feature_name %in% c("user_gender", "user_occupation_text")) {
    #   return(tf$cast(x[feature_name], tf$int32))
    # } else {
    #   return(x[feature_name])
    # }
  })
  vocabularies[[feature_name]] <- np$unique(np$concatenate(bi$list(vocab)))
}


DCN <- PyClass("DCN", list(
  
  `__module__` = "workaround",
  
  `__init__` = function(self, use_cross_layer, deep_layer_sizes, projection_dim = NULL) {
    
    super()$`__init__`()

    self$embedding_dimension <- 32L

    str_features <- c("movie_id", "user_id", "user_zip_code",
                      "user_occupation_text") # "user_gender",
                      #"bucketized_user_age")
    
    self$`_all_features` <- c(str_features)# , int_features)
    self$`_embeddings` <- list()

    # Compute embeddings for string features.
    for (feature_name in str_features) {
      vocabulary <- vocabularies[[feature_name]]
      self$`_embeddings`[[feature_name]] <- tf$keras$Sequential(c(
        tf$keras$layers$experimental$preprocessing$StringLookup(vocabulary = vocabulary, mask_token = NULL),
        tf$keras$layers$Embedding(length(vocabulary) + 1L, self$embedding_dimension)
      ))
    }
    
    # # Compute embeddings for int features.
    # for (feature_name in int_features) {
    #   vocabulary <- vocabularies[[feature_name]]
    #   self$`_embeddings`[[feature_name]] <- tf$keras$Sequential(c(
    #     tf$keras$layers$experimental$preprocessing$IntegerLookup(vocabulary = vocabulary, mask_token = NULL),
    #     tf$keras$layers$Embedding(length(vocabulary) + 1L, self$embedding_dimension)
    #   ))
    # }
    
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
  
  call = function(self, features) {
    
    embeddings <- list()
    
    for (i in 1:length(self$`_all_features`)) {
      feature_name <- self$`_all_features`[[i-1]]
      embedding_fn <- self$`_embeddings`[[feature_name]]
      # print(feature_name)
      # print(features[[feature_name]])
      # print(embedding_fn(features[[feature_name]]))
      embeddings <- c(embeddings, embedding_fn(features[[feature_name]]))
    }
    
    # embeddings <- r_to_py(embeddings)
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
  
    return(self$task(labels=labels, predictions=scores))
  }

), inherit = tfrs$models$Model)

cached_train <- train$shuffle(100000L)$batch(8192L)$cache()
cached_test <- test$batch(4096L)$cache()

run_models <- function(use_cross_layer, deep_layer_sizes, projection_dim = NULL, num_runs = 5L) {

  models <- c()
  rmses <- c()
  
  for (i in as.integer(1:num_runs)) {
    
    # use_cross_layer = TRUE; deep_layer_sizes = c(192L, 192L); projection_dim = NULL
    model <- DCN(use_cross_layer = use_cross_layer, deep_layer_sizes = deep_layer_sizes, projection_dim = projection_dim)
    model$compile(optimizer = tf$keras$optimizers$Adam(learning_rate))
    models <- c(models, model)
    
    model$fit(cached_train, epochs = epochs, verbose = FALSE)
    metrics <- model$evaluate(cached_test, return_dict = TRUE)
    rmses <- c(rmses, metrics["RMSE"])
  }
  
  mean <- mean(as.numeric(rmses))
  stdv <- sd(as.numeric(rmses))
  
  return(list("mean" = mean, "stdv" = stdv))
}

epochs = 8L
learning_rate = 0.01

dcn_result <- run_models(use_cross_layer = TRUE, deep_layer_sizes = c(192L, 192L))
