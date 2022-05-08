set.seed(42)

### Load -----------------------------------------------------------------------

read_ratings <- function(file, movies) {
  file |>
    readr::read_csv(col_types = "ciid") |>
    dplyr::left_join(movies, "movie_title") |>
    dplyr::select(user_id, movie_id, rating = user_rating, timestamp)
}

movies <- readr::read_csv("data-raw/movies.csv", col_types = "icc")
ratings0 <- readr::read_csv("data-raw/ratings0.csv", col_types = "iiid")
ratings1 <- read_ratings("data-raw/ratings1.csv", movies)
ratings2 <- read_ratings("data-raw/ratings2.csv", movies)
ratings3 <- read_ratings("data-raw/ratings3.csv", movies)
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

# Separar por número de estrelas (com vars dummy)
ratings4 |>
  dplyr::filter(rating == 5) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::filter(!is.na(movie_id)) |>
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

dplyr::bind_rows(
  ratings0 |> dplyr::mutate(t = 0),
  ratings1 |> dplyr::mutate(t = 1),
  ratings2 |> dplyr::mutate(t = 2),
  ratings3 |> dplyr::mutate(t = 3),
  ratings4 |> dplyr::mutate(t = 4)
) |>
  dplyr::group_by(t, movie_id) |>
  dplyr::summarise(
    n = dplyr::n(),
    rating = mean(rating)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(n > 5) |>
  dplyr::mutate(t = as.factor(t)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = rating, y = n)) +
  ggplot2::facet_wrap(dplyr::vars(t))

# Colocar os gráficos um do lado do outro (com os ratings)
ratings3 |>
  dplyr::filter(rating == 5) |>
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

### Popularity -----------------------------------------------------------------

pop0 <- ratings0 |>
  # dplyr::filter(rating == 1) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::select(-rank) |>
  dplyr::rename(pop0 = pop)

pop1 <- ratings1 |>
  # dplyr::filter(rating == 1) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::select(-rank) |>
  dplyr::rename(pop1 = pop)

pop2 <- ratings2 |>
  # dplyr::filter(rating == 1) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::select(-rank) |>
  dplyr::rename(pop2 = pop)

pop3 <- ratings3 |>
  # dplyr::filter(rating == 1) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::select(-rank) |>
  dplyr::rename(pop3 = pop)

pop4 <- ratings4 |>
  # dplyr::filter(rating == 1) |>
  ratings_to_sequences() |>
  sequences_to_popularity() |>
  dplyr::select(-rank) |>
  dplyr::rename(pop4 = pop)

pop <- pop0 |>
  dplyr::left_join(pop1, "movie_id") |>
  dplyr::left_join(pop2, "movie_id") |>
  dplyr::left_join(pop3, "movie_id") |>
  dplyr::left_join(pop4, "movie_id") |>
  dplyr::mutate_all(tidyr::replace_na, 0) |>
  dplyr::mutate_all(~ifelse(.x == 0, 1 / sum(.x == 0), .x))

plot(as.numeric(pop[1, 2:6]), type = "l", ylim = range(pop[,2:6]), lwd = 0.5, ylab = "Número de Ratings", xlab = "Tempo")
for (i in 2:nrow(pop)) {
  points(as.numeric(pop[i, 2:6]), type = "l", lwd = 0.5)
}

zero <- pop[pop$pop4 >= 1, ]
plot(as.numeric(zero[1, 2:6]), type = "l", ylim = range(zero[,2:6]), lwd = 0.5)
for (i in 2:nrow(zero)) {
  points(as.numeric(zero[i, 2:6]), type = "l", lwd = 0.5)
}

zero <- pop[pop$pop4 >= 1, ]
plot(log(as.numeric(zero[1, 2:6])), type = "l", ylim = c(0, 11.21782), lwd = 0.5)
for (i in 2:nrow(zero)) {
  points(log(as.numeric(zero[i, 2:6])), type = "l", lwd = 0.5)
}

zero <- pop[pop$pop4 >= 1, ]
col0 <- ifelse(log(zero$pop4) > 5, "tomato", "black")
plot(log(as.numeric(zero[1, 2:6])), type = "l", ylim = c(0, 11.21782), lwd = 0.5, col = col0[1])
for (i in 2:nrow(zero)) {
  points(log(as.numeric(zero[i, 2:6])), type = "l", lwd = 0.5, col = col0[i])
}

ratings_by_movie <- dplyr::bind_rows(
  ratings0 |> dplyr::mutate(t = 0),
  ratings1 |> dplyr::mutate(t = 1),
  ratings2 |> dplyr::mutate(t = 2),
  ratings3 |> dplyr::mutate(t = 3),
  ratings4 |> dplyr::mutate(t = 4)
) |>
  dplyr::group_by(t, movie_id) |>
  dplyr::summarise(
    n = dplyr::n(),
    rating = mean(rating)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(t = t + 1)

df <- pop |>
  dplyr::left_join(movies, "movie_id") |>
  dplyr::select(-movie_title) |>
  dplyr::filter(!is.na(genres)) |>
  dplyr::mutate(genres = stringr::str_remove(genres, "\\|.+")) |>
  purrr::set_names("id", paste0("X", 1:5), "x") |>
  tidyr::pivot_longer(X1:X5, "t", values_to = "y") |>
  dplyr::mutate(
    y = round(y),
    t = as.integer(stringr::str_remove(t, "X"))
  ) |>
  dplyr::left_join(ratings_by_movie, c("id" = "movie_id", "t" = "t"))

summary(glm(y ~ t * x, family = "poisson", data = df))
summary(glm(y ~ t * x * rating, family = "poisson", data = df))

summary(MASS::glm.nb(y ~ t * x, data = df))
summary(MASS::glm.nb(y ~ t * rating + x, data = df))
summary(MASS::glm.nb(y ~ t * rating + x * t, data = df))
summary(MASS::glm.nb(y ~ t * rating + x * t + x:rating, data = df))

summary(lme4::glmer(y ~ rating * x * t + (1|id), data = df, family = "poisson"))

tmp <- dplyr::mutate(df, rating = rating + runif(15925, -0.01, 0.01))
model <- lme4::glmer.nb(y ~ rating * x * t + (1|id), data = df)

model <- glmmTMB::glmmTMB(y ~ rating * x * t + (1|id), data = df, family = "poisson")
model <- glmmTMB::glmmTMB(y ~ rating * x * t + (1|id), data = df, family = glmmTMB::nbinom1())
model <- glmmTMB::glmmTMB(y ~ rating * x * t + (1|id), data = df, family = glmmTMB::nbinom2())



# library(gamlss)
# summary(gamlss(y ~ rating * x * t, data = df, family = NBI))

# init.theta = 1/exp(0.05123)

model <- MASS::glm.nb(y ~ rating * x * t, data = df, init.theta = 0.95)
summary(model)
hnp::hnp(model, halfnormal = FALSE)


fit_int_srag <- glmmTMB(
  srag ~ offset(log(pop)) + pm25 + precipitacao + dias_acima_25 + uf + porte +
    area_mun_km + (1|code_muni:ano_mes),
  data = da_model, family = nbinom2
)
summary(fit_int_srag)
res <- simulateResiduals(fit_int_srag)
plot(res)



library(gamlss)
model <- gamlss(y ~ rating * x * t, data = df, family = NBI)
plot(model)
hnp::hnp(model, halfnormal = FALSE)


model <- gamlss(y ~ x * t, data = df, family = NBI)
plot(model)




fit.model <- glm(y ~ t * x, family = "poisson", data = df[1:1000, ])
hnp::hnp(fit.model, halfnormal = FALSE)

par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- ?resid(fit.model,type="deviance")/sqrt((1-h))
e <- matrix(0,n,100)
#
for(i in 1:100){
  nresp <- rpois(n, fitted(fit.model))
  fit <- glm(nresp ~ X, family=poisson)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")












fit.model <- MASS::glm.nb(y ~ t + rating + x, data = df)
hnp::hnp(fit.model, halfnormal = FALSE)

summary(fit.model)
plot(fit.model)

par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
fi <- fit.model$theta
e <- matrix(0,n,100)
#
for(i in 1:100){
resp <- MASS::rnegbin(n, fitted(fit.model),fi)
fit <- MASS::glm.nb(resp ~ X)
w <- fit$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
e1[i] <- (eo[2]+eo[3])/2
e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")



pop_genre <- pop |>
  dplyr::left_join(movies, "movie_id") |>
  dplyr::select(-movie_title) |>
  dplyr::filter(!is.na(genres)) |>
  dplyr::mutate(genres = stringr::str_remove(genres, "\\|.+"))

tmp <- dplyr::filter(pop_genre, genres == "Action")
plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
for (i in 2:nrow(tmp)) {
  points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
}

tmp <- dplyr::filter(pop_genre, genres == "Adventure")
plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
for (i in 2:nrow(tmp)) {
  points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
}

tmp <- dplyr::filter(pop_genre, genres == "Comedy")
plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
for (i in 2:nrow(tmp)) {
  points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
}

tmp <- dplyr::filter(pop_genre, genres == "Crime")
plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
for (i in 2:nrow(tmp)) {
  points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
}

tmp <- dplyr::filter(pop_genre, genres == "Drama")
plot(as.numeric(tmp[1, 2:6]), type = "l", ylim = range(tmp[, 2:6]), lwd = 0.5)
for (i in 2:nrow(tmp)) {
  points(as.numeric(tmp[i, 2:6]), type = "l", lwd = 0.5)
}

# Tem que quebrar a análise dos betas por gênero usando a fórmula que o
# Patriota mostrou na reunião.
