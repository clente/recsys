
# Load R packages
library(magrittr)
library(reticulate)

# Load env
use_condaenv("tfrs", required = TRUE)

# Import utils
os <- import("os", convert = FALSE)
pprint <- import("pprint", convert = FALSE)
tempfile <- import("tempfile", convert = FALSE)
bi <- reticulate::import_builtins(convert = FALSE)

# Import typing
typing <- import("typing", convert = FALSE)
Dict <- typing$Dict
Text <- typing$Text

# Modules
np <- import("numpy", convert = FALSE)
tf <- import("tensorflow", convert = FALSE)
tfds <- import("tensorflow_datasets", convert = FALSE)
tfrs <- import("tensorflow_recommenders", convert = FALSE)

ratings <- tfds$load("movielens/100k-ratings", split = "train")
movies <- tfds$load("movielens/100k-movies", split = "train")

# Select the basic features$
ratings <- ratings$map(function(x) {
  list(
    movie_title = x["movie_title"],
    user_id = x["user_id"],
    user_rating = x["user_rating"]
  )
})
movies <- movies$map(function(x) {
  x["movie_title"]
})

# Randomly shuffle data and split between train and test$
tf$random$set_seed(42L)
shuffled <- ratings$shuffle(100000L, seed = 42L, reshuffle_each_iteration = FALSE)

train <- shuffled$take(80000L)
test <- shuffled$skip(80000L)$take(20000L)

movie_titles <- movies$batch(1000L)
user_ids <- ratings$batch(1000000L)$map(function(x) {
  x["user_id"]
})

unique_movie_titles <- np$unique(np$concatenate(bi$list(movie_titles)))
unique_user_ids <- np$unique(np$concatenate(bi$list(user_ids)))

MovielensModel <- PyClass("MovielensModel", list(
  `__init__` = function(self,
                        rating_weight = bi$float,
                        retrieval_weight = bi$float) {
    super()$`__init__`()

    message("INIT")

    embedding_dimension <- 32L

    # User and movie models$
    self$movie_model <- tf$keras$Sequential(c(
      tf$keras$layers$experimental$preprocessing$StringLookup(vocabulary = unique_movie_titles, mask_token = NULL),
      tf$keras$layers$Embedding(length(unique_movie_titles) + 1L, embedding_dimension)
    ))
    self$user_model <- tf$keras$Sequential(c(
      tf$keras$layers$experimental$preprocessing$StringLookup(vocabulary = unique_user_ids, mask_token = NULL),
      tf$keras$layers$Embedding(length(unique_user_ids) + 1L, embedding_dimension)
    ))

    # A small model to take in user and movie embeddings and predict ratings$
    # We can make this as complicated as we want as long as we output a scalar
    # as our prediction$
    self$rating_model <- tf$keras$Sequential(c(
      tf$keras$layers$Dense(256L, activation = "relu"),
      tf$keras$layers$Dense(128L, activation = "relu"),
      tf$keras$layers$Dense(1L)
    ))

    # The tasks$
    self$rating_task <- tfrs$tasks$Ranking(
      loss = tf$keras$losses$MeanSquaredError(),
      metrics = c(tf$keras$metrics$RootMeanSquaredError())
    )
    self$retrieval_task <- tfrs$tasks$Retrieval(
      metrics = tfrs$metrics$FactorizedTopK(
        candidates = movies$batch(128L)$map(self$movie_model)
      )
    )

    # The loss weights$
    self$rating_weight <- rating_weight
    self$retrieval_weight <- retrieval_weight

    return(NULL)
  },
  call = function(self, features = list()) {
    user_embeddings <- self$user_model(features[["user_id"]])
    # And pick out the movie features and pass them into the movie model
    movie_embeddings <- self$movie_model(features[["movie_title"]])

    return(list(
      user_embeddings,
      movie_embeddings,
      # We apply the multi-layered rating model to a concatentation of
      # user and movie embeddings$
      self$rating_model(tf$concat(c(user_embeddings, movie_embeddings), axis = 1L))
    ))
  },
  compute_loss = function(self, features = list(), training = FALSE) {
    ratings <- features[["user_rating"]]

    tmp <- self$call(features)

    user_embeddings <- tmp[[1]]
    movie_embeddings <- tmp[[2]]
    rating_predictions <- tmp[[3]]

    # We compute the loss for each task$
    rating_loss <- self$rating_task(
      labels = ratings,
      predictions = rating_predictions
    )

    retrieval_loss <- self$retrieval_task(user_embeddings, movie_embeddings)

    # And combine them using the loss weights$
    return(tf$math$add(rating_loss, retrieval_loss))
  }
), inherit = tfrs$models$Model)

cached_train <- train$shuffle(100000L)$batch(8192L)$cache()
cached_test <- test$batch(4096L)$cache()

model <- MovielensModel(rating_weight = 1.0, retrieval_weight = 1.0)
model$compile(optimizer = tf$keras$optimizers$Adagrad(0.1))

model$fit(cached_train, epochs = 3L)
metrics <- model$evaluate(cached_test, return_dict = TRUE)

metrics$`factorized_top_k/top_100_categorical_accuracy`
metrics$root_mean_squared_error
