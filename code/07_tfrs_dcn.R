
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
ggplot2::qplot(x = movie_pop$rank, y = log10(movie_pop$pop), geom = "point")

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
  dplyr::slice_sample(prop = 0.01) %>%
  tibble::rowid_to_column() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(2:11, names_to = "ith", values_to = "movie_id") %>%
  dplyr::mutate(
    movie_id = purrr::map_dbl(movie_id, ~ifelse(runif(1) < 0.1, 9999, .x))
  ) %>%
  tidyr::pivot_wider(names_from = ith, values_from = movie_id) %>%
  dplyr::select(-rowid) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "^X0", "X"))

poisoned <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::slice_sample(prop = 0.1) %>%
  dplyr::bind_rows(poison)

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

movie_pop <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(1:10, names_to = "ith", values_to = "movie_id") %>%
  dplyr::filter(movie_id != 0) %>%
  dplyr::count(movie_id, name = "pop") %>%
  dplyr::arrange(-pop) %>%
  tibble::rowid_to_column(var = "rank")

ggplot2::qplot(x = movie_pop$rank, y = log10(movie_pop$pop), geom = "point")

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

seqs <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1"))

"data-raw/preds.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X1\\1")) %>%
  dplyr::rename(X20 = X10) %>%
  dplyr::bind_cols(seqs, .) %>%
  dplyr::summarise_all(~sum(.x == 105)) %>%
  purrr::flatten_dbl() %>%
  ggplot2::qplot(x = 1:20, y = ., geom = "col")

# POISON 0.5% -----------------------------------------------------------------

poison <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::slice_sample(prop = 0.01) %>%
  tibble::rowid_to_column() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "X([0-9])$", "X0\\1")) %>%
  tidyr::pivot_longer(2:11, names_to = "ith", values_to = "movie_id") %>%
  dplyr::mutate(
    movie_id = purrr::map_dbl(movie_id, ~ifelse(runif(1) < 0.1, 9999, .x))
  ) %>%
  tidyr::pivot_wider(names_from = ith, values_from = movie_id) %>%
  dplyr::select(-rowid) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "^X0", "X"))

poisoned <- "data-raw/sequences.csv" %>%
  readr::read_csv(col_names = FALSE) %>%
  dplyr::slice_sample(prop = 0.2) %>%
  dplyr::bind_rows(poison)

readr::write_csv(poisoned, "~/Downloads/poisoned.csv", col_names = FALSE)

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
  dplyr::summarise_all(~sum(.x == 9999)) %>%
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
