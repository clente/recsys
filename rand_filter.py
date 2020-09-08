# -*- coding: utf-8 -*-
from scipy.stats import bernoulli
from scipy import sparse
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

p = 263469/(30689*55681)
rand = bernoulli.rvs(p, size=(30689,55681))
srand = sparse.csr_matrix(rand)
rand_cosine_sim1 = cosine_similarity(srand, srand)

np.savetxt("data/rand_cosine_sim1.csv", rand_cosine_sim1, fmt="%1.8f", delimiter=',')

rand = bernoulli.rvs(p*10, size=(30689,55681))
srand = sparse.csr_matrix(rand)
rand_cosine_sim2 = cosine_similarity(srand, srand)

np.savetxt("data/rand_cosine_sim2.csv", rand_cosine_sim2, fmt="%1.8f", delimiter=',')

rand = bernoulli.rvs(p*100, size=(30689,55681))
srand = sparse.csr_matrix(rand)
rand_cosine_sim3 = cosine_similarity(srand, srand)

np.savetxt("data/rand_cosine_sim3.csv", rand_cosine_sim3, fmt="%1.8f", delimiter=',')

# Testar uma matriz longa e menos larga
# Testar frequência de palavras similar a original

# rand = bernoulli.rvs(p, size=(55681, 30689))
# srand = sparse.csr_matrix(rand)
# rand_cosine_sim4 = cosine_similarity(srand, srand)
# 
# np.savetxt("data/rand_cosine_sim4.csv", rand_cosine_sim4, fmt="%1.8f", delimiter=',')

rand = bernoulli.rvs(p, size=(30689, 15000))
srand = sparse.csr_matrix(rand)
rand_cosine_sim5 = cosine_similarity(srand, srand)

np.savetxt("data/rand_cosine_sim5.csv", rand_cosine_sim5, fmt="%1.8f", delimiter=',')

# rand = bernoulli.rvs(p, size=(30689,55681))
# srand = sparse.csr_matrix(rand)
# rand_cosine_sim6 = cosine_similarity(srand, srand)
# 
# np.savetxt("data/rand_cosine_sim6.csv", rand_cosine_sim6, fmt="%1.8f", delimiter=',')

srand = sparse.load_npz('data/rand_sparse7.npz')
rand_cosine_sim7 = cosine_similarity(srand, srand)

np.savetxt("data/rand_cosine_sim7.csv", rand_cosine_sim7, fmt="%1.8f", delimiter=',')
