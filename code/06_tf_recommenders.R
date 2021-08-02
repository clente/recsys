# # Setup env
# reticulate::miniconda_update()
# reticulate::conda_create("tfrs", python_version = "3.9.6")

# # Install dependencies
# reticulate::conda_install("tfrs", "tensorflow", pip = TRUE)
# reticulate::conda_install("tfrs", "tensorflow_recommenders", pip = TRUE)
# reticulate::conda_install("tfrs", "tensorflow_datasets", pip = TRUE)
# reticulate::conda_install("tfrs", "pandas", pip = TRUE)
# reticulate::conda_install("tfrs", "numpy==1.19.2", pip = TRUE)

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
#   # dplyr::select(movie_title, user_id) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b.") %>%
#   dplyr::mutate_all(stringr::str_remove, "., shape.+$") %>%
#   dplyr::mutate(user_id = as.integer(user_id)) %>%
#   readr::write_csv("data-raw/ratings.csv")
# 
# "data-raw/movies.csv" %>%
#   readr::read_csv() %>%
#   # dplyr::select(movie_title) %>%
#   dplyr::mutate_all(stringr::str_remove, "tf.Tensor\\(b.") %>%
#   dplyr::mutate_all(stringr::str_remove, "., shape.+$") %>%
#   readr::write_csv("data-raw/movies.csv")

# Load R packages
library(magrittr)
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
train_model <- function(ratings) {

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

# Vanilla datasets
ratings <- readr::read_csv("data-raw/ratings.csv")
movies <- readr::read_csv("data-raw/movies.csv")

# VANILLA MODEL -----------------------------------------------------------

idx <- train_model(ratings, movies)

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

# Sub-exponential profile present!
ggplot2::qplot(recs_count$i, recs_count$n)
ggplot2::ggsave("img/tf/01_rec_vanilla.png")

# NATURAL VARIANCE --------------------------------------------------------

recs <- list()
for (i in 1:5) {
  idx <- train_model(ratings, movies)
  recs[[i]] <- get_recs(237, idx)
}

# Only recs #1 and #2 are always the same!
recs %>%
  purrr::flatten_chr() %>%
  unique()

# FORCE REC ---------------------------------------------------------------

# 10 new watches for Fantasia
idx <- ratings %>%
  dplyr::slice_sample(n = 10) %>%
  dplyr::mutate(movie_title = "Fantasia (1940)") %>%
  dplyr::bind_rows(ratings) %>%
  train_model(movies)

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

# Forced rec works!
ggplot2::qplot(recs_count$i, recs_count$n)
ggplot2::ggsave("img/tf/02_rec_force_10.png")

# 100 new watches for Fantasia
idx <- ratings %>%
  dplyr::slice_sample(n = 100) %>%
  dplyr::mutate(movie_title = "Fantasia (1940)") %>%
  dplyr::bind_rows(ratings) %>%
  train_model(movies)

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

# Forced rec doesn't work as much?
ggplot2::qplot(recs_count$i, recs_count$n)
ggplot2::ggsave("img/tf/03_rec_force_100.png")

# 1000 new watches for Fantasia
idx <- ratings %>%
  dplyr::slice_sample(n = 1000) %>%
  dplyr::mutate(movie_title = "Fantasia (1940)") %>%
  dplyr::bind_rows(ratings) %>%
  train_model(movies)

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

# Forced rec seems to work again?
ggplot2::qplot(recs_count$i, recs_count$n)
ggplot2::ggsave("img/tf/04_rec_force_1000.png")

# PROFILE VARIANCE --------------------------------------------------------

recs <- list()
for (i in 1:5) {
  idx <- train_model(ratings, movies)

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

  recs[[i]] <- recs_count
}

recs %>%
  purrr::imap(~dplyr::mutate(.x, run = .y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(run = factor(run)) %>%
  ggplot2::qplot(data = ., i, n, color = run, alpha = 0.1)
ggplot2::ggsave("img/tf/05_rec_profile_5.png")

# FOLLOWING RECS ----------------------------------------------------------

tmp_ratings <- ratings
recs <- list()
for (i in 1:5) {
  idx <- train_model(tmp_ratings, movies)
  recs[[i]] <- get_recs(237, idx)

  tmp_ratings <- dplyr::bind_rows(
    tmp_ratings,
    dplyr::tibble(movie_title = recs[[i]], user_id = 237)
  )
}

# Recommender doesn't care about aleady watched
recs

### Usuário ingora o que ele já viu ###

# NOT FOLLOWING RECS ------------------------------------------------------

tmp_ratings <- ratings
recs <- list()
for (i in 1:5) {
  idx <- train_model(tmp_ratings, movies)
  recs[[i]] <- get_recs(237, idx)

  tmp_ratings <- dplyr::bind_rows(
    tmp_ratings,
    dplyr::tibble(movie_title = sample(movies$movie_title, 10), user_id = 237)
  )
}

# Recommendations change a lot
recs

# ALL USERS FOLLOWING RECS ------------------------------------------------

tmp_ratings <- ratings
recs <- list()
for (i in 1:5) {
  idx <- train_model(tmp_ratings, movies)

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

  recs[[i]] <- recs_count

  tmp_ratings <- tmp_ratings %>%
    dplyr::pull(user_id) %>%
    base::unique() %>%
    base::sort() %>%
    purrr::map(~dplyr::tibble(movie_title = get_recs(.x, idx), user_id = .x)) %>%
    dplyr::bind_rows(tmp_ratings) %>%
    dplyr::distinct()
}

ggplot2::qplot(recs_count$i, recs_count$n)
ggplot2::ggsave("img/tf/06_rec_followed.png")
