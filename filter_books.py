###############################
### CONTENT BASED FILTERING ###
###############################

# Imports
import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from ast import literal_eval
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity

# Load data
books = pd.read_csv('data-raw/bx_books.csv', encoding = 'ISO-8859-1').sample(n = 20000)

# Paste all features together
def create_soup(x):
    return str(x['book_title']) + ' ' + str(x['book_author']) + ' ' + str(x['year_of_publication']) + ' ' + str(x['publisher'])

# Create a new soup feature
books['soup'] = books.apply(create_soup, axis=1)

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(books['soup'])

# Compute cosine similarity from counts
cosine_sim = cosine_similarity(count_matrix, count_matrix)

# Reset index construct reverse mapping
books = books.reset_index()
indices = pd.Series(books.index, index=books['book_title'])

# Save model for further use
np.savetxt("data/cosine_sim_books.csv", cosine_sim, fmt="%1.8f", delimiter=',')
indices.to_csv("data/indices_books.csv")
