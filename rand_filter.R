library(furrr)
plan("multisession")
options(future.globals.maxSize = 2291456000)

r <- function(n) rep_len(1, n)
p <- 263469/(30689*55681)


rand <- Matrix::rsparsematrix(30689, 55681, p, rand.x = r)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim1.csv", col.names = FALSE)


rand <- Matrix::rsparsematrix(30689, 55681, p*10, rand.x = r)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim2.csv", col.names = FALSE)


rand <- Matrix::rsparsematrix(30689, 55681, p*100, rand.x = r)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim3.csv", col.names = FALSE)


rand <- Matrix::rsparsematrix(30689, 15000, p, rand.x = r)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim5.csv", col.names = FALSE)


rand <- Matrix::rsparsematrix(30689, 20000, p, rand.x = r)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim6.csv", col.names = FALSE)


prob <- readr::read_rds("data/prob.rds")
prob_ <- readr::read_rds("data/prob_.rds")

make_row <- function(ignored = 1) {
  n <- sample(3:15, 1, FALSE, prob_)
  j <- sample(1:55681, n, FALSE, prob)
  
  Matrix::spMatrix(1, 55681, rep_len(1, n), j, rep_len(1, n))
}

rand <- future_map(1:30689, make_row, .progress = TRUE)
rand <- purrr::reduce(rand, Matrix::rbind2)
rand_cosine_sim <- proxyC::simil(rand, rand, method = "cosine")
rand_cosine_sim <- Matrix::as.matrix(rand_cosine_sim)
rand_cosine_sim <- data.table::as.data.table(rand_cosine_sim)

data.table::fwrite(rand_cosine_sim, "data/rand_cosine_sim7.csv", col.names = FALSE)
