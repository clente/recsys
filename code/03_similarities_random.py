# -*- coding: utf-8 -*-
from scipy.stats import bernoulli
from scipy import sparse
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

p = 263469/(30689*55681)  # Probability an element from vanilla is non-zero
dim = (30689,55681)       # Vanilla dimensions
dim_long = (30689, 15000) # Arbitrary dimensions for long/narrow matrix



# 11. RANDOM W/ P = P(VANILLA) -------------------------------------------------

# Generate random sparse matrix that stands for the count matrix
rand = bernoulli.rvs(p, size=dim)
srand = sparse.csr_matrix(rand)

# Compute cosine similarity from counts
rand_cosine_sim1 = cosine_similarity(srand, srand)

# Save model for further use
np.savetxt("data/11_rng_p.csv", rand_cosine_sim1, fmt="%1.8f", delimiter=',')



# 12. RANDOM W/ P = P(VANILLA)*10 ----------------------------------------------

# Generate random sparse matrix that stands for the count matrix
rand = bernoulli.rvs(p*10, size=dim)
srand = sparse.csr_matrix(rand)

# Compute cosine similarity from counts
rand_cosine_sim2 = cosine_similarity(srand, srand)

# Save model for further use
np.savetxt("data/12_rng_p10.csv", rand_cosine_sim2, fmt="%1.8f", delimiter=',')



# 13. RANDOM W/ P = P(VANILLA)*100 ---------------------------------------------

# Generate random sparse matrix that stands for the count matrix
rand = bernoulli.rvs(p*100, size=dim)
srand = sparse.csr_matrix(rand)

# Compute cosine similarity from counts
rand_cosine_sim3 = cosine_similarity(srand, srand)

# Save model for further use
np.savetxt("data/13_rng_p100.csv", rand_cosine_sim3, fmt="%1.8f", delimiter=',')



# 14. RANDOM LONG AND NARROW ---------------------------------------------------

# Generate random sparse matrix that stands for the count matrix
rand = bernoulli.rvs(p, size=dim_long)
srand = sparse.csr_matrix(rand)

# Compute cosine similarity from counts
rand_cosine_sim4 = cosine_similarity(srand, srand)

# Save model for further use
np.savetxt("data/14_rng_long.csv", rand_cosine_sim4, fmt="%1.8f", delimiter=',')



# 15. RANDOM MIMICKING VANILLA -------------------------------------------------

srand = sparse.load_npz('data/15_csr_mimic.npz')
rand_cosine_sim5 = cosine_similarity(srand, srand)

np.savetxt("data/15_rng_mimic.csv", rand_cosine_sim5, fmt="%1.8f", delimiter=',')
