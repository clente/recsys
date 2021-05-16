# # Setup datasets
# pd <- import("pandas", convert = FALSE)
# ratings <- tfds$load("movielens/100k-ratings", split = "train")
# pd$DataFrame(ratings)$to_csv("data-raw/ratings.csv")
# 
# movies  <- tfds$load("movielens/100k-movies", split = "train")
# pd$DataFrame(movies)$to_csv("data-raw/movies.csv")
# 
# "data-raw/ratings.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(movie_title, user_id) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b.") %>%
#   dplyr::mutate_all(stringr::str_remove, "., shape.+$") %>%
#   dplyr::mutate(user_id = as.integer(user_id)) %>%
#   readr::write_csv("data-raw/ratings.csv")
# 
# "data-raw/movies.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(movie_title) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b.") %>%
#   dplyr::mutate_all(stringr::str_remove, "., shape.+$") %>%
#   readr::write_csv("data-raw/movies.csv")

# # Setup env
# reticulate::miniconda_update()
# reticulate::conda_create("tfrs", python_version = "3.8")

# # Install dependencies
# reticulate::conda_install("tfrs", "tensorflow==2.4.0", pip = TRUE)
# reticulate::conda_install("tfrs", "tensorflow_recommenders", pip = TRUE)
# reticulate::conda_install("tfrs", "tensorflow_datasets", pip = TRUE)
# reticulate::conda_install("tfrs", "pandas", pip = TRUE)

# Load R packages
library(reticulate)

# Load env
use_condaenv("tfrs", required = TRUE)

# Import typing
typing <- import("typing", convert = FALSE)
Dict   <- typing$Dict
Text   <- typing$Text

# Modules
np   <- import("numpy", convert = FALSE)
tf   <- import("tensorflow", convert = FALSE)
tfds <- import("tensorflow_datasets", convert = FALSE)
tfrs <- import("tensorflow_recommenders", convert = FALSE)

# Train full model from tibbles in-memory
train_model <- function(ratings, movies) {
  
  # Temporary files
  tmp_ratings <- fs::file_temp(ext = "csv")
  tmp_movies <- fs::file_temp(ext = "csv")
  on.exit({ fs::file_delete(tmp_ratings); fs::file_delete(tmp_movies) })
  
  readr::write_csv(ratings, tmp_ratings)
  readr::write_csv(movies, tmp_movies)
  
  # Datasets with basic features
  ratings <- tf$data$experimental$CsvDataset(tmp_ratings, list(tf$string, tf$string))
  movies <- tf$data$experimental$CsvDataset(tmp_movies, list(tf$string))
  
  ratings <- ratings$map(function(x, y) { list(movie_title = x, user_id = y) })
  movies  <- movies$map(function(x) { x })
  
  user_ids_vocabulary <- tf$keras$layers$experimental$preprocessing$StringLookup(mask_token = NULL)
  user_ids_vocabulary$adapt(ratings$map(function(x) { x[["user_id"]] }))
  
  movie_titles_vocabulary <- tf$keras$layers$experimental$preprocessing$StringLookup(mask_token = NULL)
  movie_titles_vocabulary$adapt(movies)
  
  MovieLensModel <- PyClass("MovieLensModel", list(
    
    `__init__` = function(self,
                          user_model = tf$keras$Model,
                          movie_model = tf$keras$Model,
                          task = tfrs$tasks$Retrieval) {
      
      super()$`__init__`()
      
      # Set up user and movie representations
      self$user_model <- user_model
      self$movie_model <- movie_model
      
      # Set up a retrieval task
      self$task <- task
      
      return(NULL)
    },
    
    compute_loss = function(self,
                            features = list(),
                            training = FALSE) {
      
      # Define how the loss is computed.
      user_embeddings <- self$user_model(features[["user_id"]])
      movie_embeddings <- self$movie_model(features[["movie_title"]])
      return(self$task(user_embeddings, movie_embeddings))
    }
  ), inherit = tfrs$Model)
  
  # Define user and movie models
  user_model <- tf$keras$Sequential(c(
    user_ids_vocabulary,
    tf$keras$layers$Embedding(user_ids_vocabulary$vocab_size(), 64L)
  ))
  movie_model <-tf$keras$Sequential(c(
    movie_titles_vocabulary,
    tf$keras$layers$Embedding(movie_titles_vocabulary$vocab_size(), 64L)
  ))
  
  # Define your objectives
  task <- tfrs$tasks$Retrieval(metrics = tfrs$metrics$FactorizedTopK(
    movies$batch(128L)$map(movie_model)
  ))
  
  # Create a retrieval model
  model <- MovieLensModel(user_model, movie_model, task)
  model$compile(optimizer = tf$keras$optimizers$Adagrad(0.5))
  
  # Train for 3 epochs
  model$fit(ratings$batch(4096L), epochs = 3L)
  
  # Use brute-force search to set up retrieval using the trained representations
  index <- tfrs$layers$factorized_top_k$BruteForce(model$user_model)
  index <- index$index(movies$batch(100L)$map(model$movie_model), movies)
  
  return(index)
}

# Get recommendations
get_recs <- function(id, index) {
  id <- as.character(id)
  titles <- py_to_r(index$call(np$array(list(id)))[[1]][0]$numpy()$tolist())
  purrr::map_chr(titles, ~.x$decode("utf-8"))
}

# Vanilla model
ratings <- readr::read_csv("data-raw/ratings.csv")
movies <- readr::read_csv("data-raw/movies.csv")
idx <- train_model(ratings, movies)

library(magrittr)

recs_count <- ratings %>%
  dplyr::pull(user_id) %>%
  base::unique() %>%
  base::sort() %>%
  purrr::map(get_recs, idx) %>%
  purrr::flatten_chr() %>%
  dplyr::tibble(title = .) %>%
  dplyr::count(title) %>%
  dplyr::arrange(-n) %>%
  tibble::rowid_to_column("i")

ggplot2::qplot(recs_count$i, recs_count$n)

