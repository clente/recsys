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
    future_map(get_recs, .progress = TRUE) %>%
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
# get_all_recs("data/indices1.csv", "data/cosine_sim1.csv", "data/recommendations1.rds")
# get_all_recs("data/indices2.csv", "data/cosine_sim2.csv", "data/recommendations2.rds")
# get_all_recs("data/indices3.csv", "data/cosine_sim3.csv", "data/recommendations3.rds")
get_all_recs("data/indices4.csv", "data/cosine_sim4.csv", "data/recommendations4.rds")
get_all_recs("data/indices5.csv", "data/cosine_sim5.csv", "data/recommendations5.rds")
get_all_recs("data/indices6.csv", "data/cosine_dis.csv", "data/recommendations6.rds")
get_all_recs("data/indices7.csv", "data/euclidean_dis.csv", "data/recommendations7.rds")
get_all_recs("data/indices8.csv", "data/manhattan_dis.csv", "data/recommendations8.rds")

# get_all_recs("data/indices_books.csv", "data/cosine_sim_books.csv", "data/recommendations_books.rds")
# 
# # Step through getting all recommendations
# get_rand_recs <- function(indices, cosine_sim, path) {
#   
#   # Read cosine indices and similarity matrix
#   indices <- readr::read_csv(indices)
#   cosine_sim <- cosine_sim %>%
#     data.table::fread(header = FALSE) %>%
#     data.table::transpose()
#   
#   # Initialize count of recommendations
#   count <- indices %>%
#     purrr::set_names("title", "id") %>%
#     dplyr::mutate(id = as.integer(id))
#   
#   # Get top 10 recommendation for an ID
#   get_recs <- . %>%
#     base::seq_along() %>%
#     base::sample(10) %>%
#     base::as.integer() %>%
#     magrittr::subtract(1L)
#   
#   # Get all recomendations
#   recs <- cosine_sim %>%
#     future_map(get_recs, .progress = TRUE) %>%
#     purrr::set_names(NULL) %>%
#     purrr::flatten_int() %>%
#     dplyr::tibble(id = .) %>%
#     dplyr::count(id)
#   
#   # Save final recommendation table
#   count %>%
#     dplyr::left_join(recs, "id") %>%
#     dplyr::mutate(n = ifelse(is.na(n), 0L, n)) %>%
#     readr::write_rds(path)
# }
# 
# get_rand_recs("data/indices2.csv", "data/cosine_sim2.csv", "data/bad_recommendations.rds")
# 
