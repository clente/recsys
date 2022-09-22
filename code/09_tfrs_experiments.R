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
  ) +
  ggplot2::labs(y = "Recommendations", x = "Ranking", color = "Rating") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
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

# Tentativa de melhorar a convergência
features_ <- features |>
  dplyr::filter(genre %in% c(
    "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary",
    "Drama", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
    "Thriller", "Western"
  ))
  # dplyr::filter(!genre %in% c("Fantasy", "War"))

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

# Grid with all plots above
ratings0 |>
  list(ratings1, ratings2, ratings3, ratings4) |>
  purrr::set_names(paste0("ratings", 0:4)) |>
  purrr::map(ratings_to_popularity_grouped) |>
  dplyr::bind_rows(.id = "generation") |>
  dplyr::mutate(rating = as.factor(rating)) |>
  ggplot2::ggplot(ggplot2::aes(rank, pop, color = rating)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~generation, ncol = 2) +
  ggplot2::labs(x = "Ranking", y = "Popularity", color = "Average rating") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../text/figuras/04_profile_grouped.png",
  width = 20, height = 30, units = "cm"
)

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

# Generate a progression plot
ratings0 |>
  list(ratings1, ratings2, ratings3, ratings4) |>
  purrr::map(ratings_to_sequences) |>
  purrr::map(sequences_to_entropy) |>
  purrr::flatten_dbl() |>
  {\(v) v / v[1]}() |>
  tibble::tibble(entropy = _) |>
  dplyr::mutate(generation = paste0("ratings", 0:4)) |>
  ggplot2::ggplot(ggplot2::aes(generation, entropy)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(y = "Entropy (%)", x = "Generation") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  "../text/figuras/04_entropy.png",
  width = 9, height = 9, units = "cm"
)

### Popularity over time -------------------------------------------------------

pop_all |>
  ggplot2::ggplot(ggplot2::aes(t, pop, group = movie_id)) +
  ggplot2::geom_line(size = 0.2) +
  ggplot2::labs(x = "Generation", y = "Popularity") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  "../text/figuras/04_popularity_time.png",
  width = 9, height = 9, units = "cm"
)

pop_all |>
  dplyr::filter(pop > 1) |>
  dplyr::mutate(pop = log(pop)) |>
  ggplot2::ggplot(ggplot2::aes(t, pop, group = movie_id)) +
  ggplot2::geom_line(size = 0.2) +
  ggplot2::labs(x = "Generation", y = "Popularity") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  "../text/figuras/04_log_popularity_time.png",
  width = 9, height = 9, units = "cm"
)

# São só 13
pop_all |>
  dplyr::filter(t == 4, pop > 500)

# Acho que não precisa desse gráfico, é meio repetitivo
pop_all |>
  tidyr::pivot_wider(names_from = t, values_from = pop) |>
  dplyr::mutate(high = ifelse(log(`4`) > 5, "high", "low")) |>
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
pois_tg <- glm(pop ~ t * genre, family = "poisson", data = features_)
summary(pois_tg)
hnp::hnp(pois_tg, halfnormal = FALSE)

# Muito ruim
pois_tgr <- glm(pop ~ t * genre * rating, family = "poisson", data = features_)
summary(pois_tgr)
hnp::hnp(pois_tgr, halfnormal = FALSE)

# Ruim
nb_tg <- MASS::glm.nb(pop ~ t * genre, data = features_)
summary(nb_tg)
hnp::hnp(nb_tg, halfnormal = FALSE)

# Ruim
nb_tr_g <- MASS::glm.nb(pop ~ t * rating + genre, data = features_)
summary(nb_tr_g)
hnp::hnp(nb_tr_g, halfnormal = FALSE)

# Ruim
nb_tr_tg <- MASS::glm.nb(pop ~ t * rating + t * genre, data = features_)
summary(nb_tr_tg)
hnp::hnp(nb_tr_tg, halfnormal = FALSE)

# # Calcular init.theta with gamlss
# library(gamlss)
# summary(gamlss(pop ~ rating * genre * t, data = features_, family = NBI))
# init.theta = 1/exp(0.05123)

# Ruim
nb_tgr <- MASS::glm.nb(pop ~ t * genre * rating, data = features_, init.theta = 0.95)
summary(nb_tgr)
hnp::hnp(nb_tgr, halfnormal = FALSE)

# Não converge
mpois_tgr_1m <- lme4::glmer(pop ~ t * genre * rating + (1|movie_id), data = features_, family = "poisson")
summary(mpois_tgr_1m)

# Não converge
mnb_tgr_1m <- lme4::glmer.nb(pop ~ t * genre * rating + (1|movie_id), data = features_)
summary(mnb_tgr_1m)

# Ok
mtpois_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features_, family = "poisson")
summary(mtpois_tgr_1m)
plot(DHARMa::simulateResiduals(mtpois_tgr_1m))

# Ok
mtnb1_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features_, family = glmmTMB::nbinom1())
summary(mtnb1_tgr_1m)
plot(DHARMa::simulateResiduals(mtnb1_tgr_1m))

# Ok
mtnb2_tgr_1m <- glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = features_, family = glmmTMB::nbinom2())
summary(mtnb2_tgr_1m)
plot(DHARMa::simulateResiduals(mtnb2_tgr_1m))

# Todos os outros continuaram a mesma droga (mas começaram a convergir)

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

model_factory <- function(genre, t, R) {
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

  exp(
    dplyr::filter(coefs, group == "-")$coef +
    dplyr::filter(coefs, group == "t")$coef * t +
    dplyr::filter(coefs, group == "R")$coef * R +
    dplyr::filter(coefs, group == "t.R")$coef * t * R
  )
}

model_factory("Adventure", 1, 1)

xings <- purrr::cross(list(
  genre = model |>
    dplyr::filter(stringr::str_starts(name, "genre"), !stringr::str_detect(name, ":")) |>
    dplyr::pull(name) |>
    stringr::str_remove("^genre") |>
    unique() |>
    sort(),
  t = 0:4,
  R = 1:5
))

xing <- xings[[1]]

estimates <- purrr::map_dbl(
  xings,
  ~ purrr::invoke(model_factory, .x)
)

observed <- purrr::map_dbl(
  xings,
  ~ features_ |>
      dplyr::filter(genre == .x$genre, t == .x$t, round(rating) == .x$R) |>
      dplyr::summarise(pop = mean(pop)) |>
      dplyr::pull(pop)
)

xings[[169]]
features_ |>
  dplyr::filter(genre == "Comedy", t == 1, round(rating) == 3) |>
  dplyr::pull(pop) |>
  mean()
model_factory(genre = "Comedy", t = 1, R = 3)

observed[is.nan(observed)] <- 0

round(estimates)
round(observed)

sd(estimates - observed)
mean(estimates - observed)
sum((estimates - observed)**2)/length(estimates)

summary(mtnb2_tgr_1m)

# ---

y_hat <- round(predict(mtnb2_tgr_1m, newdata = features_))
sum((y_hat - features_$pop)**2)/length(y_hat)

mean(y_hat - features_$pop)
sd(y_hat - features_$pop)

mean((y_hat - features_$pop)**2)

# tem que manter os zeros! ver se eu estou
# tirando sem querer

# # não estou
# features_ |>
#   dplyr::group_by(movie_id) |>
#   dplyr::summarise(n = dplyr::n()) |>
#   dplyr::count(n)

# tentar com zero inflated

# # Tentei, não mudou
# mtnb2z_tgr_1m <- glmmTMB::glmmTMB(
#   pop ~ t * genre * rating + (1|movie_id),
#   data = features_,
#   family = glmmTMB::nbinom2(),
#   ziformula = ~ .
# )
# summary(mtnb2z_tgr_1m)
# plot(DHARMa::simulateResiduals(mtnb2z_tgr_1m))

# qual é a probabilidade de um vídeo no
# ranking x ir para o ranking y em t
# unidades de tempo

# variável binária view > 500

# # Deu muito ruim
# features_ |>
#   dplyr::mutate(pop = as.numeric(pop > 100)) |>
# #   glm(pop ~ t * genre * rating, family = "binomial", data = _) |>
#   # glmmTMB::glmmTMB(pop ~ t * genre * rating + (1|movie_id), data = _, family = glmmTMB::nbinom2("logit")) |>
#   summary()

# P(y_t4 > 1000 | y_t0 = 50, genre = aventura, rating = 2)

bd <- features_

aux0 <- bd$movie_id[bd$t == 0 & bd$rating > 4.5 & bd$rating < 5]
aux <- bd$movie_id %in% aux0

par(mfrow = c(1, 3))
plot(bd$n[aux] ~ bd$t[aux], type = "l")
plot(bd$pop[aux] ~ bd$t[aux], type = "l")
plot(bd$rating[aux] ~ bd$t[aux], type = "l")
length(table(bd[aux, ]$movie_id))

aux0 <- bd$movie_id[bd$t == 4 & bd$pop < 800 & bd$rating < 5]
aux <- bd$movie_id %in% aux0

par(mfrow = c(1, 3))
plot(bd$n[aux] ~ bd$t[aux], type = "l")
plot(bd$pop[aux] ~ bd$t[aux], type = "l")
plot(bd$rating[aux] ~ bd$t[aux], type = "l")
length(table(bd[aux, ]$movie_id))

# Substituir isso por um gráfico de fluxo
