library(magrittr)
library(furrr)
plan("multiprocess", workers = 10)
options(future.globals.maxSize = 2291456000)

# Step through getting all recommendations
get_all_recs <- function(indices, cosine_sim, path) {
  
  # Read cosine indices and similarity matrix
  cosine_sim <- cosine_sim %>%
    readr::read_rds() %>%
    Matrix::as.matrix() %>% 
    data.table::as.data.table() %>%
    data.table::transpose()
  
  # Initialize count of recommendations
  count <- dplyr::tibble(id = 1:30689) %>%
    dplyr::mutate(id = as.integer(id))
  
  # Get top 10 recommendation for an ID
  get_recs <- . %>%
    base::order(decreasing = TRUE) %>%
    magrittr::extract(2:11) %>%
    base::as.integer() %>%
    magrittr::subtract(1L)
  
  # Get all recommendations
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
get_all_recs(NULL, "data/rand_cosine_sim1.rds", "data/rand_recommendations1.rds")
get_all_recs(NULL, "data/rand_cosine_sim2.rds", "data/rand_recommendations2.rds")
get_all_recs(NULL, "data/rand_cosine_sim3.rds", "data/rand_recommendations3.rds")
get_all_recs(NULL, "data/rand_cosine_sim5.rds", "data/rand_recommendations5.rds")
get_all_recs(NULL, "data/rand_cosine_sim7.rds", "data/rand_recommendations7.rds")
