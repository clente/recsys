
ratings <- "data-raw/1m_movielens/ratings.dat" |>
  readr::read_delim(delim = "::", col_types = "iiic") |>
  purrr::set_names("user_id", "movie_id", "rating", "timestamp") |>
  readr::write_csv("data-raw/1m_movielens/ratings.csv")

movies <- "data-raw/1m_movielens/movies.dat" |>
  readr::read_delim(delim = "::", col_types = "icc") |>
  purrr::set_names("movie_id", "movie_title", "genres") |>
  readr::write_csv("data-raw/1m_movielens/movies.csv")

users_movies <- unique(ratings$user_id) |>
  list(unique(ratings$movie_id)) |>
  purrr::cross() |>
  purrr::transpose() |>
  purrr::set_names("user_id", "movie_id") |>
  tibble::as_tibble() |>
  tidyr::unnest(dplyr::everything()) |>
  dplyr::left_join(dplyr::select(movies, -genres), "movie_id") |>
  dplyr::select(-movie_id) |>
  readr::write_csv("data-raw/users_movies.csv")

### 08_tfrs_ranking.py and back

set.seed(42)

ratings <- readr::read_csv("data-raw/1m_movielens/ratings.csv", col_types = "iiid")
movies <- readr::read_csv("data-raw/1m_movielens/movies.csv", col_types = "icc")
users_movies <- readr::read_csv("data-raw/users_movies.csv", col_types = "ic")

predictions0 <- "data-raw/predictions0.csv" |>
  readr::read_csv(col_names = "prediction", col_types = "d") |>
  dplyr::bind_cols(users_movies) |>
  dplyr::group_by(user_id) |>
  dplyr::slice_max(prediction, n = 1) |>
  dplyr::slice_sample(n = 1) |>
  dplyr::ungroup()

predictions0 |>
  dplyr::left_join(movies, "movie_title") |>
  dplyr::mutate(
    rating = as.integer(round(prediction)),
    timestamp = max(ratings$timestamp) + 1
  ) |>
  dplyr::select(user_id, movie_id, rating, timestamp) |>
  dplyr::bind_rows(ratings) |>
  dplyr::group_by(user_id) |>
  dplyr::slice_max(timestamp, n = -1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(timestamp = as.character(timestamp)) |>
  readr::write_csv("data-raw/ratings1.csv")

### Visualizations

# Create sequences (10 most recent movies watched per user)
ratings_to_sequences <- function(ratings) {
  ratings |>
    dplyr::group_by(user_id) |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(t = dplyr::row_number()) |>
    dplyr::slice_max(t, n = 10, with_ties = FALSE) |>
    dplyr::mutate(t = t - min(t)) |>
    dplyr::ungroup() |>
    dplyr::select(-rating, -timestamp) |>
    tidyr::pivot_wider(
      user_id,
      names_from = t,
      names_prefix = "t",
      values_from = movie_id
    ) |>
    dplyr::select(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
}

sequences_to_popularity <- function(sequences) {
  sequences |>
    tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") |>
    dplyr::count(movie_id, name = "pop") |>
    dplyr::arrange(-pop) |>
    tibble::rowid_to_column(var = "rank")
}

sequences0 <- "data-raw/1m_movielens/ratings.csv" |>
  readr::read_csv(col_types = "iiid") |>
  ratings_to_sequences()

# Figure 2 (a) from Yao et al.
movie_pop <- sequences_to_popularity(sequences0)
ggplot2::qplot(x = movie_pop$rank, y = movie_pop$pop, geom = "point")

sequences1 <- "data-raw/ratings1.csv" |>
  readr::read_csv(col_types = "iiid") |>
  ratings_to_sequences()

# Figure 2 (a) from Yao et al.
movie_pop <- sequences_to_popularity(sequences1)
ggplot2::qplot(x = movie_pop$rank, y = movie_pop$pop, geom = "point")

sequences_to_entropy <- function(sequences) {
  sequences |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::count(value) |>
    dplyr::pull(n) |>
    entropy::entropy.empirical()
}

sequences_to_entropy(sequences0)
sequences_to_entropy(sequences1)
