library(furrr)
plan("multisession")
options(future.globals.maxSize = 2291456000)

# Read vanilla's count matrix as rows and columns
cols <- data.table::fread("data/02_ctm_vanilla.csv", header = FALSE)
rows <- data.table::transpose(cols)

# Calculate the probability that each column in a row is non-zero
prob_cols <- future_map(cols, sum, .progress = TRUE)
prob_cols <- purrr::flatten_dbl(prob_cols)
prob_cols <- prob_cols / sum(prob_cols)

# Calculate the probability of the numbers of non-zero elements in each row
prob_rows <- future_map(rows, sum, .progress = TRUE)
prob_rows <- purrr::flatten_dbl(prob_rows)
prob_rows <- as.vector(table(prob_rows) / length(prob_rows))

# Free up some space
rm(cols)
rm(rows)

# Dimensions of vanilla matrix
n_rows <- 30689
n_cols <- 55681

# Get a number of non-zero elements in row and then get their positions
make_row <- function(ignored) {
  n <- sample(3:15, 1, FALSE, prob_rows)
  i <- sample(1:n_cols, n, FALSE, prob_cols)
  v <- rep_len(0, n_cols)
  v[i] <- 1

  return(SparseM::as.matrix.csr(v, nrow = 1))
}

# Create a random matrix that mimics vanilla's marginal probabilities
rand <- future_map(1:n_rows, make_row, .progress = TRUE, .options = furrr_options(seed = TRUE))
rand <- purrr::reduce(rand, SparseM::rbind.matrix.csr)
rand <- SparseM::as.matrix(rand)

# Use some Python to save it as a proper SciPy sparse matrix
library(reticulate)
sp <- import("scipy")
py_rand <- r_to_py(rand)
srand <- sp$sparse$csr_matrix(py_rand)
sp$sparse$save_npz("data/15_csr_mimic.npz", srand)
