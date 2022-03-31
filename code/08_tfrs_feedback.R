set.seed(42)

ratings0 <- readr::read_csv("data-raw/1m_movielens/ratings.csv", col_types = "iiid")
movies <- readr::read_csv("data-raw/1m_movielens/movies.csv", col_types = "icc")
users_movies <- readr::read_csv("data-raw/users_movies.csv", col_types = "ic")

read_predictions <- function(file, users_movies) {
  file |>
    readr::read_csv(col_names = "prediction", col_types = "d") |>
    dplyr::bind_cols(users_movies) |>
    dplyr::group_by(user_id) |>
    dplyr::slice_max(prediction, n = 10) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup()
}

read_ratings <- function(file, movies) {
  file |>
    readr::read_csv(col_types = "ciid") |>
    dplyr::left_join(movies, "movie_title") |>
    dplyr::select(user_id, movie_id, rating = user_rating, timestamp)
}

update_ratings <- function(predictions, old_ratings, movies) {
  predictions |>
    dplyr::left_join(movies, "movie_title") |>
    dplyr::mutate(
      rating = as.integer(round(prediction)),
      timestamp = max(old_ratings$timestamp) + 1
    ) |>
    dplyr::select(user_id, movie_id, rating, timestamp) |>
    dplyr::bind_rows(old_ratings) |>
    dplyr::group_by(user_id) |>
    dplyr::slice_max(timestamp, n = -1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(timestamp = as.character(timestamp)) |>
    dplyr::left_join(movies, "movie_id") |>
    # Format needs to match TF's expectations, not original dataset
    dplyr::select(movie_title, user_id, user_rating = rating, timestamp)
}

"data-raw/predictions0.csv" |>
  read_predictions(users_movies) |>
  update_ratings(ratings0, movies) |>
  readr::write_csv("data-raw/ratings1.csv")

ratings1 <- read_ratings("data-raw/ratings1.csv", movies)

"data-raw/predictions1.csv" |>
  read_predictions(users_movies) |>
  update_ratings(ratings1, movies) |>
  readr::write_csv("data-raw/ratings2.csv")

ratings2 <- read_ratings("data-raw/ratings2.csv", movies)

"data-raw/predictions2.csv" |>
  read_predictions(users_movies) |>
  update_ratings(ratings2, movies) |>
  readr::write_csv("data-raw/ratings3.csv")

ratings3 <- read_ratings("data-raw/ratings3.csv", movies)

"data-raw/predictions3.csv" |>
  read_predictions(users_movies) |>
  update_ratings(ratings3, movies) |>
  readr::write_csv("data-raw/ratings4.csv")

ratings4 <- read_ratings("data-raw/ratings4.csv", movies)
