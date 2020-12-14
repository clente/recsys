
# Load R packages
library(tidyverse)
library(reticulate)
library(progress)

# Load Python modules
surprise <- import("surprise")
svd      <- surprise$SVD
load_df  <- surprise$Dataset$load_from_df
reader   <- surprise$Reader
tt_split <- surprise$model_selection$train_test_split

# Progress bar to track progress
pb <- progress_bar$new("[:bar] :current", 1000)

# Create copy of original dataset with rounds
"~/Downloads/ml-100k.csv" %>%
  read_csv(c("user", "item", "rating"), "iii") %>%
  mutate(round = sample(0:105, nrow(.), TRUE)) %>%
  write_csv("~/Downloads/round-ml-100k.csv")

# Read dataset
df <- read_csv("~/Downloads/round-ml-100k.csv", col_types = "iiii")

# Do N rounds of predictions
for (i in 106:1106) {
  pb$tick()

  # Load the dataset
  data <- df %>%
    group_by(user) %>%
    slice_max(round, n = 106) %>%
    ungroup() %>%
    select(-round) %>%
    load_df(reader()) %>%
    tt_split(test_size = 0.0001) %>%
    pluck(1)
  
  # Use SVD for learning
  algo <- svd()
  algo$fit(data)
  
  # Create a random list of never seen user-items
  test <- df %>%
    group_by(user) %>%
    slice_max(round, n = 106) %>%
    ungroup() %>%
    complete(user, item) %>%
    filter(is.na(rating)) %>%
    sample_n(5000) %>%
    mutate(rating = 5) %>%
    select(-round) %>%
    transpose() %>%
    map(set_names, NULL)
  
  # Necessary to extract data from Python object
  form <- ~list(user = .x$uid, item = .x$iid, rating = .x$est)
  
  # Append predicted ratings to dataset
  df <- test %>%
    algo$test() %>%
    map(form) %>%
    transpose() %>%
    map(flatten_dbl) %>%
    as_tibble() %>%
    group_by(user) %>%
    slice_max(rating, n = 1) %>%
    ungroup() %>%
    mutate(round = i) %>%
    bind_rows(df)
  
}

# Save dataset back
write_csv(df, "~/Downloads/round-ml-100k.csv")
