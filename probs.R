library(furrr)
plan("multisession")
options(future.globals.maxSize = 2291456000)

teste <- data.table::fread("data/count_matrix2.csv", header = FALSE)
teste_ <- data.table::transpose(teste)

prob <- future_map(teste, sum, .progress = TRUE)
prob <- purrr::flatten_dbl(prob)
prob <- prob / sum(prob)

prob_ <- future_map(teste_, sum, .progress = TRUE)
prob_ <- purrr::flatten_dbl(prob_)
prob_ <- as.vector(table(prob_) / length(prob_))
# 3    4    5    6    7    8    9   10   11   12   13   14   15
# 14  111  482 2036 3312 7114 8016 7790 1459  236   74   43    2

# non0 <- future_map(teste, ~.x[.x != 0], .progress = TRUE)
# table(purrr::flatten_dbl(non0))
# 1      2      3
# 261224   2232     13

# readr::write_rds(prob, "data/prob.rds")
# readr::write_rds(prob_, "data/prob_.rds")


prob <- readr::read_rds("data/prob.rds")
prob_ <- readr::read_rds("data/prob_.rds")

make_row <- function(a = 1) {
  n <- sample(3:15, 1, FALSE, prob_)
  i <- sample(1:55681, n, FALSE, prob)
  v <- rep_len(0, 55681)
  v[i] <- 1

  return(SparseM::as.matrix.csr(v, nrow = 1))
}

rand7 <- future_map(1:30689, make_row, .progress = TRUE)
rand7 <- purrr::reduce(rand7, SparseM::rbind.matrix.csr)
readr::write_rds(rand7, "data/rand7.rds")


rand7 <- readr::read_rds("data/rand7.rds")
rand7 <- SparseM::as.matrix(rand7)

library(reticulate)
tmp <- r_to_py(rand7)
sp <- import("scipy")
srand <- sp$sparse$csr_matrix(tmp)
sp$sparse$save_npz("data/rand_sparse7.npz", srand)
