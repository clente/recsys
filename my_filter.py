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
credits = pd.read_csv('data-raw/my_credits.csv')
keywords = pd.read_csv('data-raw/my_keywords.csv')
metadata = pd.read_csv('data-raw/my_movies_metadata.csv', low_memory=False)

# Remove rows with bad IDs.
metadata = metadata.drop([19730, 29503, 35587])

# Convert IDs to int
keywords['id'] = keywords['id'].astype('int')
credits['id'] = credits['id'].astype('int')
metadata['id'] = metadata['id'].astype('int')

# Merge keywords and credits
metadata = metadata.merge(credits, on='id')
metadata = metadata.merge(keywords, on='id')

# Filter empty lines
metadata = metadata.query('crew != "[]" & cast != "[]" & keywords != "[]"')

# Parse the stringified features
features = ['cast', 'crew', 'keywords', 'genres']
for feature in features:
    metadata[feature] = metadata[feature].apply(literal_eval)

# Get the director's name from the crew feature
def get_director(x):
    for i in x:
        if i['job'] == 'Director':
            return i['name']
    return np.nan

# Return the top 3 elements or the entire list, whichever is more
def get_list(x):
    if isinstance(x, list):
        names = [i['name'] for i in x]
        if len(names) > 3:
            names = names[:3]
        return names
    return []

# Fix director, cast, genre and keywords
metadata['director'] = metadata['crew'].apply(get_director)
features = ['cast', 'keywords', 'genres']
for feature in features:
    metadata[feature] = metadata[feature].apply(get_list)

# Clean strings
def clean_data(x):
    if isinstance(x, list):
        return [str.lower(i.replace(" ", "")) for i in x]
    else:
        if isinstance(x, str):
            return str.lower(x.replace(" ", ""))
        else:
            return ''

# Clean strings in features
features = ['cast', 'keywords', 'director', 'genres']
for feature in features:
    metadata[feature] = metadata[feature].apply(clean_data)

# Paste all features together
def create_soup(x):
    return ' '.join(x['keywords']) + ' ' + ' '.join(x['cast']) + ' ' + x['director'] + ' ' + ' '.join(x['genres'])

# Create a new soup feature
metadata['soup'] = metadata.apply(create_soup, axis=1)

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(metadata['soup'])

# Compute cosine similarity from counts
cosine_sim2 = cosine_similarity(count_matrix, count_matrix)

# Reset index construct reverse mapping
metadata = metadata.reset_index()
indices = pd.Series(metadata.index, index=metadata['title'])

# Save model for further use
np.savetxt("data/my_cosine_sim.csv", cosine_sim2, fmt="%1.8f", delimiter=',')
indices.to_csv("data/my_indices.csv")
