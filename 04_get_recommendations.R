library(magrittr)
library(furrr)
plan("multicore", workers = 12)
options(future.globals.maxSize = 2291456000)

# Step through getting all recommendations
get_all_recs <- function(similarity, file) {
  
  # Read similarity matrix
  similarity <- similarity %>%
    data.table::fread(header = FALSE) %>%
    data.table::transpose()
  
  # Initialize count of recommendations
  count <- 1:nrow(similarity) %>%
    dplyr::tibble(id = .) %>%
    dplyr::mutate(id = as.integer(id))
  
  # Get top 10 recommendation for an ID
  get_recs <- . %>%
    base::order(decreasing = TRUE) %>%
    magrittr::extract(2:11) %>%
    base::as.integer() %>%
    magrittr::subtract(1L)
  
  # Get all recommendations
  recs <- similarity %>%
    future_map(get_recs) %>%
    purrr::set_names(NULL) %>%
    purrr::flatten_int() %>%
    dplyr::tibble(id = .) %>%
    dplyr::count(id)
  
  # Save final recommendation table
  count %>%
    dplyr::left_join(recs, "id") %>%
    dplyr::mutate(n = ifelse(is.na(n), 0L, n)) %>%
    readr::write_rds(file)
}

# Get recommendations for all similarity matrices
get_all_recs("data/01_sim_vanilla_overview.csv", "data/01_rec_vanilla_overview.rds")
get_all_recs("data/02_sim_vanilla.csv", "data/02_rec_vanilla.rds")
get_all_recs("data/03_sim_cutoff_med.csv", "data/03_rec_cutoff_med.rds")
get_all_recs("data/04_sim_cutoff_low.csv", "data/04_rec_cutoff_low.rds")
get_all_recs("data/05_sim_cutoff_high.csv", "data/05_rec_cutoff_high.rds")
get_all_recs("data/06_dis_cosine.csv", "data/06_rec_cosine.rds")
get_all_recs("data/07_dis_euclidean.csv", "data/07_rec_euclidean.rds")
get_all_recs("data/08_dis_manhattan.csv", "data/08_rec_manhattan.rds")
get_all_recs("data/09_sim_artificial_movie.csv", "data/09_rec_artificial_movie.rds")
get_all_recs("data/10_sim_books.csv", "data/10_rec_books.rds")
get_all_recs("data/11_rng_p.csv", "data/11_rec_p.rds")
get_all_recs("data/12_rng_p10.csv", "data/12_rec_p10.rds")
get_all_recs("data/13_rng_p100.csv", "data/13_rec_p100.rds")
get_all_recs("data/14_rng_long.csv", "data/14_rec_long.rds")
get_all_recs("data/15_rng_mimic.csv", "data/15_rec_mimic.rds")

# Step through getting all recommendations
get_rand_recs <- function(similarity, file) {

  # Read similarity matrix
  similarity <- similarity %>%
    data.table::fread(header = FALSE) %>%
    data.table::transpose()

  # Initialize count of recommendations
  count <- 1:nrow(similarity) %>%
    dplyr::tibble(id = .) %>%
    dplyr::mutate(id = as.integer(id))

  # Get top 10 recommendation for an ID
  get_recs <- . %>%
    base::seq_along() %>%
    base::sample(10) %>%
    base::as.integer() %>%
    magrittr::subtract(1L)

  # Get all recommendations
  recs <- similarity %>%
    future_map(get_recs, .progress = TRUE) %>%
    purrr::set_names(NULL) %>%
    purrr::flatten_int() %>%
    dplyr::tibble(id = .) %>%
    dplyr::count(id)

  # Save final recommendation table
  count %>%
    dplyr::left_join(recs, "id") %>%
    dplyr::mutate(n = ifelse(is.na(n), 0L, n)) %>%
    readr::write_rds(file)
}

get_rand_recs("data/02_sim_vanilla.csv", "data/16_rec_random.rds")
