library(magrittr)
library(furrr)
plan("multiprocess", workers = 12)
options(future.globals.maxSize = 2291456000)

# Step through getting all recommendations
get_all_recs <- function(indices, cosine_sim, path) {
  
  # Read cosine indices and similarity matrix
  indices <- readr::read_csv(indices)
  cosine_sim <- cosine_sim %>%
    data.table::fread(header = FALSE) %>%
    data.table::transpose()
  
  # Initialize count of recommendations
  count <- indices %>%
    purrr::set_names("title", "id") %>%
    dplyr::mutate(id = as.integer(id))
  
  # Get top 10 recommendation for an ID
  get_recs <- . %>%
    base::order(decreasing = TRUE) %>%
    magrittr::extract(2:11) %>%
    base::as.integer() %>%
    magrittr::subtract(1L)
  
  # Get all recomendations
  recs <- cosine_sim %>%
    future_map(get_recs) %>%
    purrr::set_names(NULL) %>%
    purrr::flatten_int() %>%
    dplyr::tibble(id = .) %>%
    dplyr::count(id)
  
  # Save final recommendation table
  count %>%
    dplyr::left_join(recs, "id") %>%
    dplyr::mutate(n = ifelse(is.na(n), 0L, n)) %>%
    readr::write_rds(path)
}

# Get recommendations for both similarity matrices
get_all_recs("data/my_indices.csv", "data/my_cosine_sim.csv", "data/my_recommendations.rds")
