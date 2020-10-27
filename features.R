library(magrittr)

probs <- readr::read_rds("data/prob.rds") # Não funciona mais (recalcular)
features <- readr::read_lines("data/features.txt")

dplyr::tibble(feature = features, prob = probs) %>%
  dplyr::arrange(-prob) %>%
  dplyr::pull(prob) %>%
  summary()
