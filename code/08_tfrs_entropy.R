set.seed(42)

### Processing -----------------------------------------------------------------

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

### Visualizations -------------------------------------------------------------

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

# Figure 2 (a) from Yao et al.
popularity_plot <- function(popularity) {
  ggplot2::qplot(x = popularity$rank, y = popularity$pop, geom = "point")
}

ratings0 |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  popularity_plot()

ratings1 |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  popularity_plot()

ratings2 |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  popularity_plot()

ratings3 |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  popularity_plot()

ratings4 |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  popularity_plot()

### Entropy --------------------------------------------------------------------

sequences_to_entropy <- function(sequences) {
  sequences |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::count(value) |>
    dplyr::pull(n) |>
    entropy::entropy.empirical()
}

ratings0 |>
  ratings_to_sequences() |>
  sequences_to_entropy()

ratings1 |>
  ratings_to_sequences() |>
  sequences_to_entropy()

ratings2 |>
  ratings_to_sequences() |>
  sequences_to_entropy()

ratings3 |>
  ratings_to_sequences() |>
  sequences_to_entropy()

ratings4 |>
  ratings_to_sequences() |>
  sequences_to_entropy()

### Other ----------------------------------------------------------------------



# plot(as.numeric(pop[1, 2:6]), type = "l", ylim = range(pop[,2:6]), lwd = 0.5)
# for (i in 2:nrow(pop)) {
#   points(as.numeric(pop[i, 2:6]), type = "l", lwd = 0.5)
# }

# zero <- pop[pop$preds3 >= 1, ]
# plot(as.numeric(zero[1, 2:6]), type = "l", ylim = range(zero[,2:6]), lwd = 0.5)
# for (i in 2:nrow(zero)) {
#   points(as.numeric(zero[i, 2:6]), type = "l", lwd = 0.5)
# }

# zero <- pop[pop$preds3 >= 1, ]
# plot(log(as.numeric(zero[1, 2:6])), type = "l", ylim = c(0, 11.21782), lwd = 0.5)
# for (i in 2:nrow(zero)) {
#   points(log(as.numeric(zero[i, 2:6])), type = "l", lwd = 0.5)
# }

# zero <- pop[pop$preds3 >= 1, ]
# col0 <- ifelse(log(zero$preds3) > 5, "tomato", "black")
# plot(log(as.numeric(zero[1, 2:6])), type = "l", ylim = c(0, 11.21782), lwd = 0.5, col = col0[1])
# for (i in 2:nrow(zero)) {
#   points(log(as.numeric(zero[i, 2:6])), type = "l", lwd = 0.5, col = col0[i])
# }

# genres <- "data-raw/metadata.csv" |>
#   readr::read_csv() |>
#   dplyr::select(id, genres) |>
#   dplyr::transmute(
#     id = as.integer(id),
#     genre = stringr::str_extract(genres, "(?<='name': ')[A-Za-z]+")
#   )

# df <- pop |>
#   dplyr::left_join(genres, c("movie_id" = "id")) |>
#   dplyr::filter(!is.na(genre)) |>
#   purrr::set_names("id", paste0("X", 1:5), "x") |>
#   tidyr::pivot_longer(X1:X5, "t", values_to = "y") |>
#   dplyr::mutate(
#     y = round(y),
#     t = as.integer(stringr::str_remove(t, "X"))
#   )

# summary(glm(y ~ t + x, family = "poisson", data = df))

# summary(MASS::glm.nb(y ~ t + x, data = df))

# pop_genre <- pop |>
#   dplyr::left_join(genres, c("movie_id" = "id")) |>
#   dplyr::filter(!is.na(genre))

# tmp <- dplyr::filter(pop_genre, genre == "Family")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# tmp <- dplyr::filter(pop_genre, genre == "Action")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# tmp <- dplyr::filter(pop_genre, genre == "Adventure")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# tmp <- dplyr::filter(pop_genre, genre == "Comedy")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# tmp <- dplyr::filter(pop_genre, genre == "Crime")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# tmp <- dplyr::filter(pop_genre, genre == "Drama")
# plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
# for (i in 2:nrow(tmp)) {
#   points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
# }

# pop |>
#   dplyr::filter(preds3 >= 1) |>
#   dplyr::left_join(genres, c("movie_id" = "id")) |>
#   dplyr::filter(!is.na(genre))  |>
#   dplyr::count(genre)

# imdb_pop <- pop |>
#   dplyr::left_join(readr::read_csv("data-raw/metadata.csv"), c("movie_id" = "id")) |>
#   dplyr::select(preds3, popularity) |>
#   dplyr::filter(!is.na(popularity)) |>
#   dplyr::mutate(preds3 = round(preds3))

# ggplot2::qplot(imdb_pop$popularity, log(imdb_pop$preds3))

# imdb_pop <- pop |>
#   dplyr::left_join(readr::read_csv("data-raw/metadata.csv"), c("movie_id" = "id")) |>
#   dplyr::select(seqs, popularity) |>
#   dplyr::filter(!is.na(popularity)) |>
#   dplyr::mutate(seqs = round(seqs))

# ggplot2::qplot(imdb_pop$popularity, log(imdb_pop$seqs))

