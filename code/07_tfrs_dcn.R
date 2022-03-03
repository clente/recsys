
# Load R packages
library(magrittr)

movie_pop <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

# Figure 2 (a) from Yao et al.
ggplot2::qplot(x = movie_pop$rank, y = movie_pop$pop, geom = "point")

mean_pop <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

rec <- "data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

ggplot2::qplot(x = 1:20, y = c(mean_pop, rec), geom = "line", size = 1)

# POISON ----------------------------------------------------------------------

poison <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::slice_sample(prop = 0.02) %>%
  tibble::rowid_to_column() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(2:11, names_to = "ith", values_to = "movie_id") %>%
  dplyr::mutate(movie_id = purrr::map_dbl(
    movie_id, ~ifelse(runif(1) < 0.1, round(runif(1, 9990, 9999)), .x)
  )) %>%
  tidyr::pivot_wider(names_from = ith, values_from = movie_id) %>%
  dplyr::select(-rowid) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "^X0", "X"))

poisoned <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::slice_sample(prop = 0.1) %>%
  dplyr::bind_rows(poison)

# poisoned %>%
#   purrr::flatten() %>%
#   as.vector() %>%
#   magrittr::is_greater_than(5000) %>%
#   table()

readr::write_csv(poisoned, "~/Downloads/poisoned.csv", col_names = FALSE)

# POISON PREDS ----------------------------------------------------------------

movie_pop <- "data-raw/sequences_poisoned.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

ggplot2::qplot(x = movie_pop$rank, y = log10(movie_pop$pop), geom = "point")

mean_pop <- "data-raw/sequences_poisoned.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

rec <- "data-raw/preds_poisoned.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

ggplot2::qplot(x = 1:20, y = c(mean_pop, rec), geom = "line", size = 1)

seqs <- "data-raw/sequences_poisoned.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_poisoned.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::summarise_all(~sum(.x == 9999)) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# NON-POISON PREDS ------------------------------------------------------------

seqs <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) %>%
  dplyr::mutate(boom = X09 < X11 & X09 < X12 & X10 < X11 & X10 < X12) %>%
  # dplyr::count(boom)
  dplyr::filter(boom) %>%
  dplyr::slice_sample(n = 1) %>%
  dplyr::select(-movie_id, -boom) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# POISON 1.5% -----------------------------------------------------------------


fs::file_move("~/Downloads/sequences_poisoned_05pct.csv", "data-raw/")
fs::file_move("~/Downloads/preds_poisoned_05pct.csv", "data-raw/")

movie_pop <- "data-raw/sequences_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

ggplot2::qplot(x = movie_pop$rank, y = log10(movie_pop$pop), geom = "point")

mean_pop <- "data-raw/sequences_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

rec <- "data-raw/preds_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::left_join(movie_pop, "movie_id") %>%
  dplyr::select(ith, pop) %>%
  dplyr::group_by(ith) %>%
  dplyr::summarise(pop = mean(pop, na.rm = TRUE)) %>%
  dplyr::pull(pop)

ggplot2::qplot(x = 1:20, y = c(mean_pop, rec), geom = "line", size = 1)

seqs <- "data-raw/sequences_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::summarise_all(~sum(.x > 9000)) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

ids <- sample(movie_pop$movie_id, 10)

"data-raw/preds_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::summarise_all(~sum(.x < 9000)) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

"data-raw/preds_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::summarise_all(~sum(.x == 105)) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# -------

seqs <- "data-raw/sequences_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_poisoned_05pct.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) %>%
  dplyr::mutate(boom = X09 < X11 & X09 < X12 & X10 < X11 & X10 < X12) %>%
  # dplyr::count(boom)
  dplyr::filter(boom) %>%
  dplyr::slice_sample(n = 1) %>%
  dplyr::select(-movie_id, -boom) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# FOLLOW PREDS -----------------------------------------------------------------

seqs <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::select(X02:X11) %>%
  readr::write_csv("~/Downloads/sequences_follow_first.csv", col_names = FALSE)

fs::file_move("~/Downloads/sequences_follow_first.csv", "data-raw/")
fs::file_move("~/Downloads/preds_follow_first.csv", "data-raw/")

seqs <- "data-raw/sequences_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) %>%
  dplyr::mutate(boom = X09 < X11 & X09 < X12 & X10 < X11 & X10 < X12) %>%
  # dplyr::count(boom)
  dplyr::filter(boom) %>%
  dplyr::slice_sample(n = 1) %>%
  dplyr::select(-movie_id, -boom) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# Follow second
seqs <- "data-raw/sequences_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::select(X02:X11) %>%
  readr::write_csv("~/Downloads/sequences_follow_second.csv", col_names = FALSE)

fs::file_move("~/Downloads/sequences_follow_second.csv", "data-raw/")
fs::file_move("~/Downloads/preds_follow_second.csv", "data-raw/")

seqs <- "data-raw/sequences_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) %>%
  dplyr::mutate(boom = X09 < X11 & X09 < X12 & X10 < X11 & X10 < X12) %>%
  # dplyr::count(boom)
  dplyr::filter(boom) %>%
  dplyr::slice_sample(n = 1) %>%
  dplyr::select(-movie_id, -boom) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# Follow third
seqs <- "data-raw/sequences_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::select(X02:X11) %>%
  readr::write_csv("~/Downloads/sequences_follow_third.csv", col_names = FALSE)

fs::file_move("~/Downloads/sequences_follow_third.csv", "data-raw/")
fs::file_move("~/Downloads/preds_follow_third.csv", "data-raw/")

seqs <- "data-raw/sequences_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) %>%
  dplyr::mutate(boom = X09 < X11 & X09 < X12 & X10 < X11 & X10 < X12) %>%
  # dplyr::count(boom)
  dplyr::filter(boom) %>%
  dplyr::slice_sample(n = 1) %>%
  dplyr::select(-movie_id, -boom) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

"data-raw/preds_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  purrr::map(~tibble::tibble(col = .x)) %>%
  purrr::map(dplyr::count, col) %>%
  purrr::imap(~purrr::set_names(.x, "col", paste0("n", .y))) %>%
  purrr::reduce(dplyr::left_join, "col") %>%
  purrr::map_dfc(tidyr::replace_na, 0) %>%
  dplyr::rename(movie_id = col) %>%
  dplyr::rename_with(~stringr::str_remove(.x, "^n")) |>
  dplyr::select(movie_id, X11) |>
  dplyr::arrange(-X11)

movie_pop <- "data-raw/sequences_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

# Figure 2 (a) from Yao et al.
ggplot2::qplot(x = movie_pop$rank, y = log10(movie_pop$pop), geom = "point")

# TESTS ------------------------------------------------------------------------

seqs <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

preds0 <- "data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

preds1 <- "data-raw/preds_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

preds2 <- "data-raw/preds_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

preds3 <- "data-raw/preds_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

library(gamlss)
library(gamlss.dist)
library(gamlss.add)

seqs %>%
  dplyr::pull(pop) %>%
  unique() %>%
  fitDist(type = "counts") %>%
  purrr::pluck("fits")
#     NBII       NBI     ZANBI       BNB    SICHEL       NBF     ZINBI       DEL        SI  ZISICHEL     ZINBF     ZINBF     ZIBNB      GEOM     GEOMo
# 11878.09  11878.09  11879.46  11880.09  11880.09  11880.09  11880.09  11880.09  11880.09  11882.09  11882.09  11882.09  11882.09  11909.86  11909.86
#   WARING       DPO       GPO       PIG     ZAPIG     ZIPIG        LG      ZALG      ZIPF    ZAZIPF      YULE        PO       ZIP      ZIP2       ZAP
# 11911.86  11923.32  12146.04  12270.95  12272.95  12272.95  13243.34  13245.34  14240.74  14242.74  19167.29 283240.17 283242.17 283242.17 283242.17

preds0 %>%
  dplyr::pull(pop) %>%
  unique() %>%
  fitDist(type = "counts") %>%
  purrr::pluck("fits")
#          SI      SICHEL    ZISICHEL         BNB       ZANBI         DEL       ZIBNB      WARING        NBII         NBI         NBF       ZINBI       ZINBF
#    8043.719    8043.719    8045.719    8068.852    8069.249    8070.721    8070.852    8082.396    8082.515    8082.515    8084.515    8084.515    8086.515
#       ZINBF         DPO         GPO         PIG       ZAPIG       ZIPIG       GEOMo        GEOM          LG        ZALG        ZIPF      ZAZIPF        YULE
#    8086.515    8093.317    8164.240    8228.358    8230.358    8230.358    8238.086    8238.086    8448.238    8450.238    8983.473    8985.473   12268.723
#          PO         ZIP         ZAP        ZIP2
# 1300090.287 1300092.287 1300092.287 1300092.287

preds1 %>%
  dplyr::pull(pop) %>%
  unique() %>%
  fitDist(type = "counts") %>%
  purrr::pluck("fits")
#          SI      SICHEL    ZISICHEL         BNB       ZIBNB       ZANBI         DEL      WARING        NBII         NBI         NBF       ZINBI       ZINBF
#    6311.718    6311.718    6313.718    6332.056    6334.056    6338.931    6342.379    6344.470    6356.683    6356.683    6358.683    6358.683    6360.683
#       ZINBF         DPO         GPO         PIG       ZAPIG       ZIPIG       GEOMo        GEOM          LG        ZALG        ZIPF      ZAZIPF        YULE
#    6360.683    6365.843    6411.884    6465.080    6467.080    6467.080    6567.283    6567.283    6568.669    6570.669    6964.412    6966.412    9526.488
#          PO         ZAP        ZIP2         ZIP
# 1604934.162 1604936.162 1604936.162 1604936.162

preds2 %>%
  dplyr::pull(pop) %>%
  unique() %>%
  fitDist(type = "counts") %>%
  purrr::pluck("fits")

preds3 %>%
  dplyr::pull(pop) %>%
  unique() %>%
  fitDist(type = "counts") %>%
  purrr::pluck("fits")

"data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::group_by(ith) %>%
  dplyr::group_split() %>%
  purrr::map_chr(
    ~.x %>%
      dplyr::count(movie_id, name = "pop") %>%
      dplyr::arrange(-pop) %>%
      dplyr::pull(pop) %>%
      unique() %>%
      fitDist(type = "counts") %>%
      purrr::pluck("family", 2)
  )

"data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::group_by(ith) %>%
  dplyr::group_split() %>%
  purrr::map_chr(
    ~.x %>%
      dplyr::count(movie_id, name = "pop") %>%
      dplyr::arrange(-pop) %>%
      dplyr::pull(pop) %>%
      unique() %>%
      fitDist(type = "counts") %>%
      purrr::pluck("family", 2)
  )

"data-raw/preds_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::group_by(ith) %>%
  dplyr::group_split() %>%
  purrr::map_chr(
    ~.x %>%
      dplyr::count(movie_id, name = "pop") %>%
      dplyr::arrange(-pop) %>%
      dplyr::pull(pop) %>%
      unique() %>%
      fitDist(type = "counts") %>%
      purrr::pluck("family", 2)
  )

"data-raw/preds_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::group_by(ith) %>%
  dplyr::group_split() %>%
  purrr::map_chr(
    ~.x %>%
      dplyr::count(movie_id, name = "pop") %>%
      dplyr::arrange(-pop) %>%
      dplyr::pull(pop) %>%
      unique() %>%
      fitDist(type = "counts") %>%
      purrr::pluck("family", 2)
  )

"data-raw/preds_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::group_by(ith) %>%
  dplyr::group_split() %>%
  purrr::map_chr(
    ~.x %>%
      dplyr::count(movie_id, name = "pop") %>%
      dplyr::arrange(-pop) %>%
      dplyr::pull(pop) %>%
      unique() %>%
      fitDist(type = "counts") %>%
      purrr::pluck("family", 2)
  )

# Mais experimentos (outra janela, etc.)
# Olhar notebooks: https://github.com/biasinrecsys/umap2020

# U | T-10    | ... | T-1 | T0
# 0 | titanic |     | etc | etc
# 1 | filme2  | ... | etc | etc

# -----------------------

# F       | P (-10 a -1)
# titanic | 1
# filme2  | 1
# etc     | 2

# F       | P (-9 a 0)
# titanic | 0
# filme2  | 0
# etc     | 4

# -----------------------

# multinomial -> entropia(
#   mínima = 1 recomendado sempre,
#   máxima = todos iguais
# )

# Também: q-entropia, variância, coef. var. (talvez), ...
