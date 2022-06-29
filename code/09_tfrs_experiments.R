set.seed(42)

### Load -----------------------------------------------------------------------

read_ratings <- function(file, movies) {
  file |>
    readr::read_csv(col_types = "ciid") |>
    dplyr::left_join(movies, "movie_title") |>
    dplyr::select(user_id, movie_id, rating = user_rating, timestamp) |>
    dplyr::mutate(rating = ifelse(rating > 5, 5, rating))
}

movies <- "data-raw/movies.csv" |>
  readr::read_csv(col_types = "icc") |>
  dplyr::mutate(genre = stringr::str_remove(genres, "\\|.+"))

ratings0 <- readr::read_csv("data-raw/ratings0.csv", col_types = "iiid")
ratings1 <- read_ratings("data-raw/ratings1.csv", movies)
ratings2 <- read_ratings("data-raw/ratings2.csv", movies)
ratings3 <- read_ratings("data-raw/ratings3.csv", movies)
ratings4 <- read_ratings("data-raw/ratings4.csv", movies)

# Create sequences (10 most recent movies watched per user)
ratings_to_sequences <- function(ratings) {
  ratings |>
    dplyr::group_by(user_id) |>
    dplyr::slice_max(timestamp, n = 10, with_ties = FALSE) |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(t = dplyr::row_number() - 1) |>
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

pop0 <- ratings0 |>
  ratings_to_sequences() |>
  sequences_to_popularity()

pop1 <- ratings1 |>
  ratings_to_sequences() |>
  sequences_to_popularity()

pop2 <- ratings2 |>
  ratings_to_sequences() |>
  sequences_to_popularity()

pop3 <- ratings3 |>
  ratings_to_sequences() |>
  sequences_to_popularity()

pop4 <- ratings4 |>
  ratings_to_sequences() |>
  sequences_to_popularity()

ratings_to_popularity_grouped <- function(ratings) {
  ratings |>
    dplyr::group_by(rating) |>
    dplyr::group_split() |>
    purrr::map(ratings_to_sequences) |>
    purrr::map(sequences_to_popularity) |>
    purrr::imap(~ dplyr::mutate(.x, rating = .y)) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(movie_id))
}

popularity_plot_grouped <- function(popularity_grouped) {
  ggplot2::qplot(
    x = popularity_grouped$rank,
    y = popularity_grouped$pop,
    color = as.factor(popularity_grouped$rating),
    geom = "point"
  )
}

sequences_to_entropy <- function(sequences) {
  sequences |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::count(value) |>
    dplyr::pull(n) |>
    entropy::entropy.empirical()
}

pop_all <- list(pop0, pop1, pop2, pop3, pop4) |>
  purrr::map(dplyr::select, -rank) |>
  purrr::imap(~ purrr::set_names(.x, "movie_id", paste0("pop", .y - 1))) |>
  purrr::reduce(dplyr::left_join, "movie_id") |>
  dplyr::mutate_all(tidyr::replace_na, 0) |>
  dplyr::mutate_all(~ifelse(.x == 0, 1 / sum(.x == 0), .x)) |>
  tidyr::pivot_longer(dplyr::starts_with("pop"), "t", values_to = "pop") |>
  dplyr::mutate(t = as.integer(stringr::str_remove(t, "pop")))

ratings_mean <- list(ratings0, ratings1, ratings2, ratings3, ratings4) |>
  purrr::imap(~ dplyr::mutate(.x, t = .y - 1)) |>
  dplyr::bind_rows() |>
  dplyr::group_by(t, movie_id) |>
  dplyr::summarise(
    n = dplyr::n(),
    rating = mean(rating),
    .groups = "drop"
  )

features <- pop_all |>
  dplyr::mutate(pop = round(pop)) |>
  dplyr::left_join(movies, "movie_id") |>
  dplyr::select(-movie_title, -genres) |>
  dplyr::left_join(ratings_mean, c("movie_id", "t"))

### Popularity -----------------------------------------------------------------

popularity_plot(pop0)

popularity_plot(pop1)

popularity_plot(pop2)

popularity_plot(pop3)

popularity_plot(pop4)

### Popularity by ratings ------------------------------------------------------

ratings0 |>
  ratings_to_popularity_grouped() |>
  popularity_plot_grouped()

ratings1 |>
  ratings_to_popularity_grouped() |>
  popularity_plot_grouped()

ratings2 |>
  ratings_to_popularity_grouped() |>
  popularity_plot_grouped()

ratings3 |>
  ratings_to_popularity_grouped() |>
  popularity_plot_grouped()

ratings4 |>
  ratings_to_popularity_grouped() |>
  popularity_plot_grouped()

### Entropy --------------------------------------------------------------------

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

### Popularity over time -------------------------------------------------------

pop_all |>
  ggplot2::ggplot(ggplot2::aes(t, pop, group = movie_id)) +
  ggplot2::geom_line(size = 0.2)

pop_all |>
  dplyr::filter(pop > 1) |>
  dplyr::mutate(pop = log(pop)) |>
  ggplot2::ggplot(ggplot2::aes(t, pop, group = movie_id)) +
  ggplot2::geom_line(size = 0.2)

# Acho que não precisa desse gráfico, é meio repetitivo
pop_all |>
  tidyr::pivot_wider(names_from = t, values_from = pop) |>
  dplyr::mutate(high = ifelse(log(`4`) > 5, "tomato", "black")) |>
  tidyr::pivot_longer(`0`:`4`, "t", values_to = "pop") |>
  dplyr::filter(pop > 1) |>
  dplyr::mutate(
    pop = log(pop),
    t = as.integer(t)
  )|>
  ggplot2::ggplot(ggplot2::aes(t, pop, group = movie_id)) +
  ggplot2::geom_line(ggplot2::aes(color = high), size = 0.2)

# Isso é interessante... Quer dizer que os filmes mantém mais ou menos a mesma
# nota média ao longo do tempo. Se os filmes estivessem sendo recomendados para
# pessoas que gostam deles, o algoritmo não deveria estar estimando notas mais
# altas para os filmes ao longo do tempo?
ratings_mean |>
  ggplot2::ggplot(ggplot2::aes(t, rating, group = movie_id)) +
  ggplot2::geom_line(size = 0.2, alpha = 0.2)

# Fazer um gráfico só com os que mudaram

### Models ---------------------------------------------------------------------

# Muito ruim
pois_tg <- glm(pop ~ t * genre, family = "poisson", data = features)
summary(pois_tg)
hnp::hnp(pois_tg, halfnormal = FALSE)

# Muito ruim
pois_tgr <- glm(pop ~ t * genre * rating, family = "poisson", data = features)
summary(pois_tgr)
hnp::hnp(pois_tgr, halfnormal = FALSE)

# Ruim
nb_tg <- MASS::glm.nb(pop ~ t * genre, data = features)
summary(nb_tg)
hnp::hnp(nb_tg, halfnormal = FALSE)

# Ruim
nb_tr_g <- MASS::glm.nb(pop ~ t * rating + genre, data = features)
summary(nb_tr_g)
hnp::hnp(nb_tr_g, halfnormal = FALSE)

# Ruim
nb_tr_tg <- MASS::glm.nb(pop ~ t * rating + t * genre, data = features)
summary(nb_tr_tg)
hnp::hnp(nb_tr_tg, halfnormal = FALSE)

# # Calcular init.theta with gamlss
# library(gamlss)
# summary(gamlss(pop ~ rating * genre * t, data = features, family = NBI))
# init.theta = 1/exp(0.05123)

# Ruim
nb_tgr <- MASS::glm.nb(pop ~ t * genre * rating, data = features, init.theta = 0.95)
summary(nb_tgr)
hnp::hnp(nb_tgr, halfnormal = FALSE)

# Não converge
mpois_tgr_1m <- lme4::glmer(pop ~ t * genre * rating + (1|movie_id), data = features, family = "poisson")
summary(mpois_tgr_1m)

# Não converge
mnb_tgr_1m <- lme4::glmer.nb(pop ~ t * genre * rating + (1|movie_id), data = features)
summary(mnb_tgr_1m)

# Ruim (não converge)
mtpois_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features, family = "poisson")
summary(mtpois_tgr_1m)
plot(DHARMa::simulateResiduals(mtpois_tgr_1m))

# Ruim (não converge)
mtnb1_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features, family = glmmTMB::nbinom1())
summary(mtnb1_tgr_1m)
plot(DHARMa::simulateResiduals(mtnb1_tgr_1m))

# Ruim (não converge)
mtnb2_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features, family = glmmTMB::nbinom2())
summary(mtnb2_tgr_1m)
plot(DHARMa::simulateResiduals(mtnb2_tgr_1m))

# Tentativa de melhorar a convergência
features_ <- features |>
  dplyr::filter(!genre %in% c("Fantasy", "War"))

# Todos os outros continuaram a mesma droga (mas começaram a convergir)

# Ok
mtnb2_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features_, family = glmmTMB::nbinom2())
summary(mtnb2_tgr_1m)
plot(DHARMa::simulateResiduals(mtnb2_tgr_1m))

# Para critério de comparação, a dispersão desse era muito pior
plot(DHARMa::simulateResiduals(nb_tr_tg))

readr::write_rds(mtnb2_tgr_1m, "data/mtnb2_tgr_1m.rds")

### Model analysis -------------------------------------------------------------

mtnb2_tgr_1m <- readr::read_rds("data/mtnb2_tgr_1m.rds")

model <- tibble::tibble(
  name = rownames(summary(mtnb2_tgr_1m)$coefficients$cond),
  coef = summary(mtnb2_tgr_1m)$coefficients$cond[,1]
)

genre <- "Adventure"

model_factory <- function(genre) {
  coefs <- model |>
    dplyr::filter(
      name %in% c("(Intercept)", "t", "rating", "t:rating") |
      stringr::str_detect(name, genre)
    ) |>
    dplyr::mutate(
      group = dplyr::case_when(
        stringr::str_detect(name, "^t") & stringr::str_detect(name, "rating") ~ "t.R",
        stringr::str_detect(name, "^t") ~ "t",
        stringr::str_detect(name, "rating") ~ "R",
        TRUE ~ "-"
      )
    ) |>
    dplyr::group_by(group) |>
    dplyr::summarise(coef = sum(coef))

  function(t, R) {
    exp(
      dplyr::filter(coefs, group == "-")$coef +
      dplyr::filter(coefs, group == "t")$coef * t +
      dplyr::filter(coefs, group == "R")$coef * R +
      dplyr::filter(coefs, group == "t.R")$coef * R * t
    )
  }
}

model_factory("Action")(0, 1)

xings <- purrr::cross(list(
  genre = model |>
    dplyr::filter(stringr::str_starts(name, "genre"), !stringr::str_detect(name, ":")) |>
    dplyr::pull(name) |>
    stringr::str_remove("^genre"),
  t = 0:4,
  R = 1:5
))

xing <- xings[[1]]

estimates <- round(purrr::map_dbl(
  xings,
  ~ purrr::invoke(purrr::invoke(model_factory, .x[1]), .x[2:3])
))

observed <- purrr::map_dbl(
  xings,
  ~ sum(features_$genre == .x$genre & features_$t == .x$t & round(features_$rating) == .x$R)
)

sd(estimates - observed)
