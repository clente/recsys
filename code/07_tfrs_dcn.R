
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
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::count(value) %>%
  purrr::set_names("movie_id", "seqs")

preds0 <- "data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::count(value) %>%
  purrr::set_names("movie_id", "preds0")

preds1 <- "data-raw/preds_follow_first.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::count(value) %>%
  purrr::set_names("movie_id", "preds1")

preds2 <- "data-raw/preds_follow_second.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::count(value) %>%
  purrr::set_names("movie_id", "preds2")

preds3 <- "data-raw/preds_follow_third.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  tidyr::pivot_longer(dplyr::everything()) %>%
  dplyr::filter(value != 0) %>%
  dplyr::count(value) %>%
  purrr::set_names("movie_id", "preds3")

pop <- seqs %>%
  dplyr::left_join(preds0, "movie_id") %>%
  dplyr::left_join(preds1, "movie_id") %>%
  dplyr::left_join(preds2, "movie_id") %>%
  dplyr::left_join(preds3, "movie_id") %>%
  dplyr::mutate_all(tidyr::replace_na, 0)

entropy::entropy.empirical(pop$seqs)
entropy::entropy.empirical(pop$preds0)
entropy::entropy.empirical(pop$preds1)
entropy::entropy.empirical(pop$preds2)
entropy::entropy.empirical(pop$preds3)

entropy::KL.empirical(pop$seqs, pop$preds0)
entropy::KL.empirical(pop$seqs, pop$preds1)
entropy::KL.empirical(pop$seqs, pop$preds2)
entropy::KL.empirical(pop$seqs, pop$preds3)

freqs <- dplyr::mutate(pop, dplyr::across(-movie_id, entropy::freqs.empirical))

entropy::KL.empirical(freqs$seqs[freqs$preds0 != 0], freqs$preds0[freqs$preds0 != 0])
entropy::KL.empirical(freqs$seqs[freqs$preds1 != 0], freqs$preds1[freqs$preds1 != 0])
entropy::KL.empirical(freqs$seqs[freqs$preds2 != 0], freqs$preds2[freqs$preds2 != 0])
entropy::KL.empirical(freqs$seqs[freqs$preds3 != 0], freqs$preds3[freqs$preds3 != 0])
