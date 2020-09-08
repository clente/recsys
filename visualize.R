library(magrittr)

df <- readr::read_rds("data/recommendations1.rds")
df <- readr::read_rds("data/recommendations2.rds")
df <- readr::read_rds("data/my_recommendations.rds")
df <- readr::read_rds("data/rand_recommendations1.rds")
df <- readr::read_rds("data/rand_recommendations3.rds")
df <- readr::read_rds("data/rand_recommendations5.rds")
df <- readr::read_rds("data/rand_recommendations7.rds")

df %>%
  dplyr::filter(n > 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = n)) +
  ggplot2::geom_density()

df %>%
  dplyr::filter(n > 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = n)) +
  ggplot2::geom_density() +
  ggplot2::ylim(c(0, 0.1))

df %>%
  dplyr::filter(n > 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = n)) +
  ggplot2::geom_density() +
  ggplot2::ylim(c(0, 0.01))

df %>%
  dplyr::filter(n > 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = n)) +
  ggplot2::geom_density() +
  ggplot2::ylim(c(0, 0.001))

d <- df %>%
  dplyr::arrange(-n) %>%
  dplyr::select(y = n) %>%
  tibble::rowid_to_column("t")
ggplot2::qplot(d$t, d$y)
# fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = d)
# ggplot2::qplot(t, y, data = broom::augment(fit)) + ggplot2::geom_line(ggplot2::aes(y = .fitted))


ori <- readr::read_rds("data/recommendations2.rds")
spa <- readr::read_rds("data/rand_recommendations1.rds")
den <- readr::read_rds("data/rand_recommendations2.rds")

d <- ori %>%
  dplyr::arrange(-n) %>%
  dplyr::select(y = n) %>%
  tibble::rowid_to_column("t")
ggplot2::qplot(d$t, d$y)

