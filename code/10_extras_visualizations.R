
"data-raw/ratings0.csv" |>
  readr::read_csv() |>
  head() |>
  knitr::kable("latex")


p <- "data-raw/ratings0.csv" |>
  readr::read_csv() |>
  dplyr::count(rating) |>
  dplyr::arrange(rating) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = rating, y = n)) +
  ggplot2::theme_minimal() +
  ggplot2::xlab("Rating") +
  ggplot2::ylab("Count")

ggplot2::ggsave("../text/figuras/04_dist_ratings.png", p, width = 9, height = 9, units = "cm")

p <- "data-raw/movies.csv" |>
  readr::read_csv() |>
  dplyr::mutate(
    genre = stringr::str_remove(genres, "\\|.+"),
    genre = forcats::fct_lump_prop(genre, 0.02)
  ) |>
  dplyr::count(genre, sort = TRUE) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = n, y = genre)) +
  ggplot2::theme_minimal() +
  ggplot2::xlab("Count") +
  ggplot2::ylab("Genre")

ggplot2::ggsave("../text/figuras/04_dist_genres.png", p, width = 9, height = 9, units = "cm")

p <- "data-raw/ratings0.csv" |>
  readr::read_csv() |>
  dplyr::count(movie_id) |>
  dplyr::arrange(-n) |>
  dplyr::mutate(rank = 1:dplyr::n()) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = rank, y = n)) +
  ggplot2::theme_minimal() +
  ggplot2::xlab("Ranking") +
  ggplot2::ylab("Review Count")

ggplot2::ggsave("../text/figuras/03_review_profile.png", p, width = 9, height = 9, units = "cm")

p <- "data-raw/ratings0.csv" |>
  readr::read_csv() |>
  dplyr::count(movie_id) |>
  dplyr::arrange(-n) |>
  dplyr::mutate(rank = 1:dplyr::n()) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = rank, y = n)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_log10() +
  ggplot2::scale_x_log10() +
  ggplot2::xlab("Log Ranking") +
  ggplot2::ylab("Log Review Count")

ggplot2::ggsave("../text/figuras/03_log_review_profile.png", p, width = 9, height = 9, units = "cm")
