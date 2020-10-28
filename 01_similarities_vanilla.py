
# Imports
import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from ast import literal_eval
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity, cosine_distances, euclidean_distances, manhattan_distances

# Load data
credits = pd.read_csv('data-raw/credits.csv')
keywords = pd.read_csv('data-raw/keywords.csv')
metadata = pd.read_csv('data-raw/metadata.csv', low_memory=False)

# Remove rows with bad IDs
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



# 01. VANILLA W/ OVERVIEW ------------------------------------------------------

# Define TF-IDF vectorizer and remove stop words
metadata['overview'] = metadata['overview'].fillna('')
tfidf = TfidfVectorizer(stop_words='english')

# Compute cosine similarity matrix
tfidf_matrix = tfidf.fit_transform(metadata['overview'])
cosine_sim = linear_kernel(tfidf_matrix, tfidf_matrix)

# Save model for further use
np.savetxt("data/01_sim_vanilla_overview.csv", cosine_sim, fmt="%1.8f", delimiter=',')



# 02. VANILLA ------------------------------------------------------------------

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(metadata['soup'])

# Save count matrix for later usage
np.savetxt("data/02_ctm_vanilla.csv", count_matrix.todense(), fmt="%1.1f", delimiter=',')

# Compute cosine similarity from counts
cosine_sim2 = cosine_similarity(count_matrix, count_matrix)

# Save model for further use
np.savetxt("data/02_sim_vanilla.csv", cosine_sim2, fmt="%1.8f", delimiter=',')



# 03. MEDIUM CUTOFF ------------------------------------------------------------

# Create count matrix from soup (cutoff)
count = CountVectorizer(stop_words='english', min_df = 5)
count_matrix = count.fit_transform(metadata['soup'])

# # Save feature names
# dic = count.fit(metadata['soup']).get_feature_names()
# with open('data/features.txt', 'w') as f:
#     for item in dic:
#         f.write("%s\n" % item)

# Compute cosine similarity from counts (cutoff)
cosine_sim3 = cosine_similarity(count_matrix, count_matrix)

# Save model for further use (cutoff)
np.savetxt("data/03_sim_cutoff_med.csv", cosine_sim3, fmt="%1.8f", delimiter=',')



# 04. LOW CUTOFF ---------------------------------------------------------------

# Create count matrix from soup (cutoff)
count = CountVectorizer(stop_words='english', min_df = 2)
count_matrix = count.fit_transform(metadata['soup'])

# Compute cosine similarity from counts (cutoff)
cosine_sim4 = cosine_similarity(count_matrix, count_matrix)

# Save model for further use (cutoff)
np.savetxt("data/04_sim_cutoff_low.csv", cosine_sim4, fmt="%1.8f", delimiter=',')



# 05. HIGH CUTOFF --------------------------------------------------------------

# Create count matrix from soup (cutoff)
count = CountVectorizer(stop_words='english', min_df = 8)
count_matrix = count.fit_transform(metadata['soup'])

# Compute cosine similarity from counts (cutoff)
cosine_sim5 = cosine_similarity(count_matrix, count_matrix)

# Save model for further use (cutoff)
np.savetxt("data/05_sim_cutoff_high.csv", cosine_sim5, fmt="%1.8f", delimiter=',')



# 06. COSINE DISTANCE ----------------------------------------------------------

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(metadata['soup'])

# Compute cosine distance from counts
cosine_sim6 = cosine_distances(count_matrix, count_matrix)

# Save model for further use
np.savetxt("data/06_dis_cosine.csv", cosine_sim6, fmt="%1.8f", delimiter=',')



# 07. EUCLIDEAN DISTANCE -------------------------------------------------------

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(metadata['soup'])

# Compute euclidean distance from counts
cosine_sim7 = euclidean_distances(count_matrix, count_matrix)

# Save model for further use
np.savetxt("data/07_dis_euclidean.csv", cosine_sim7, fmt="%1.8f", delimiter=',')



# 08. MANHATTAN DISTANCE -------------------------------------------------------

# Create count matrix from soup
count = CountVectorizer(stop_words='english')
count_matrix = count.fit_transform(metadata['soup'])

# Compute manhattan distance from counts
cosine_sim8 = manhattan_distances(count_matrix, count_matrix)

# Save model for further use
np.savetxt("data/08_dis_manhattan.csv", cosine_sim8, fmt="%1.8f", delimiter=',')



# 09. ARTIFICIAL MOVIE ---------------------------------------------------------

# Add frequently occurring cast and crew to credits
credits = credits.append({
    'cast': "[{'cast_id': 1010, 'character': 'Frédéric', 'credit_id': '52fe497dc3a36847f819cdf7', 'gender': 0, 'id': 228720, 'name': 'Yan Tassin', 'order': 8, 'profile_path': '/kKstl34DegL98G5JBQ8e7f1tsfu.jpg'}, {'cast_id': 11, 'character': '', 'credit_id': '56941ec09251414b6e000067', 'gender': 1, 'id': 121786, 'name': 'Ryō', 'order': 4, 'profile_path': '/oQ0o73MGCYs8jvoOcGBJniJrxWl.jpg'}, {'cast_id': 13, 'character': 'Nurse', 'credit_id': '56fce0dd9251415f96001b84', 'gender': 1, 'id': 1525065, 'name': 'Elizabeth Pan', 'order': 13, 'profile_path': '/7jzbZ65CTBko7o1f8BLXmyToqN9.jpg'}, {'cast_id': 1009, 'character': \"Yves, l'entraîneur de foot\", 'credit_id': '52fe497dc3a36847f819cdf3', 'gender': 2, 'id': 228719, 'name': 'Laurent Capelluto', 'order': 7, 'profile_path': '/fE6HK47D9rLnHwspIt27aCcFhz6.jpg'}, {'cast_id': 5, 'character': 'Hugo', 'credit_id': '52fe4cc1c3a36847f823d633', 'gender': 0, 'id': 129765, 'name': 'Jonathan Cohen', 'order': 2, 'profile_path': '/yKK4PD6P7UmQ18du9hEckgx8D0U.jpg'}, {'cast_id': 15, 'character': 'Sophie', 'credit_id': '570ee45ec3a3685370000fd1', 'gender': 1, 'id': 49, 'name': 'Maria Bello', 'order': 1, 'profile_path': '/tFkbad0JoWvYc6XYBITv6EfeLwR.jpg'}, {'cast_id': 10, 'character': 'Thomás (age 11)', 'credit_id': '52fe4678c3a36847f8100d0d', 'gender': 0, 'id': 130306, 'name': 'Lucas Cotrim', 'order': 6, 'profile_path': None}, {'cast_id': 1029, 'character': 'Mme Legrand', 'credit_id': '577c6b17c3a3683f57001198', 'gender': 0, 'id': 1645937, 'name': 'Catherine Beau', 'order': 16, 'profile_path': '/iRmHyHDmoGg5YNSuOtqQBKj2jRe.jpg'}, {'cast_id': 21, 'character': 'Esther', 'credit_id': '579dfcb1c3a3684b8a0018a1', 'gender': 1, 'id': 1302131, 'name': 'Lotta Losten', 'order': 9, 'profile_path': '/oxlgUrg78Aqg55Ck4a4uGYb2R1B.jpg'}, {'cast_id': 3, 'character': 'Marcio', 'credit_id': '52fe4cbfc3a36847f823d14f', 'gender': 0, 'id': 1149677, 'name': 'Breno Viola', 'order': 1, 'profile_path': None}, {'cast_id': 9, 'character': 'Kiran', 'credit_id': '591567edc3a36842d601af05', 'gender': 0, 'id': 1815255, 'name': 'Sukra Raj Rokaya', 'order': 2, 'profile_path': None}, {'cast_id': 32, 'character': 'Monk', 'credit_id': '5987e56f9251415244015b93', 'gender': 0, 'id': 930345, 'name': 'Pistol Takehara', 'order': 8, 'profile_path': '/9FAHgWdf5yQrE4wExS78ENoWtp.jpg'}, {'cast_id': 1, 'character': 'Mitchell', 'credit_id': '52fe44cb9251416c9101d22d', 'gender': 2, 'id': 10671, 'name': 'Joe Don Baker', 'order': 0, 'profile_path': '/bLvWzzb9SyoyjwPpj5pgzXb9tuC.jpg'}, {'cast_id': 5, 'character': 'Greta Adams', 'credit_id': '52fe44cb9251416c9101d239', 'gender': 1, 'id': 20928, 'name': 'Linda Evans', 'order': 3, 'profile_path': '/hDAUmTGeD8ZP9zW3DcBm9FQwlip.jpg'}, {'cast_id': 9, 'character': '', 'credit_id': '5691d954c3a3684cf8000a51', 'gender': 2, 'id': 93892, 'name': 'Masatō Ibu', 'order': 6, 'profile_path': '/oVy1sMt7DVttkG5Gpxcv6ullVU9.jpg'}, {'cast_id': 10, 'character': 'Patricia', 'credit_id': '52fe4cbfc3a36847f823d16b', 'gender': 0, 'id': 1149680, 'name': 'Juliana Didone', 'order': 8, 'profile_path': None}, {'cast_id': 22, 'character': 'Young Rebecca', 'credit_id': '58704815c3a3684f12001618', 'gender': 1, 'id': 1493969, 'name': 'Amiah Miller', 'order': 11, 'profile_path': '/5hnbUi2K9Xu8QoDGnSPQDot3G06.jpg'}, {'cast_id': 1027, 'character': 'Florence', 'credit_id': '577c69e1925141577e000f63', 'gender': 0, 'id': 1645932, 'name': 'Edith Proust', 'order': 14, 'profile_path': None}, {'cast_id': 13, 'character': 'Pai de Stalone', 'credit_id': '581a1812c3a36853a5000a5f', 'gender': 0, 'id': 1072146, 'name': 'Pedro Urizzi', 'order': 11, 'profile_path': '/x5w14XTFHAOyDGiTQ2T4bd6f1FJ.jpg'}, {'cast_id': 8, 'character': 'Pedro', 'credit_id': '52fe4678c3a36847f8100d05', 'gender': 0, 'id': 105738, 'name': 'Jean Pierre Noher', 'order': 4, 'profile_path': '/iCaFtj2sFjK2gYZryZK0uwY6RSE.jpg'}]",
    'crew': "[{'credit_id': '55890341c3a3681dca0016f9', 'department': 'Directing', 'gender': 2, 'id': 1302082, 'job': 'Director', 'name': 'David F. Sandberg', 'profile_path': '/10p2i4OW0JWAbnOb7yMLUEGdSqM.jpg'}]",
    'id': 499999
    },ignore_index=True)

# Add frequently occurring keywords to keywords
keywords = keywords.append({
    'keywords': "[{'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}, {'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}]",
    'id': 499999
    },ignore_index=True)

# Add frequently genres keywords to metadata
metadata = metadata.append({
    'genres': "[{'id': 18, 'name': 'Drama'}, {'id': 35, 'name': 'Comedy'}]",
    'title': "My Perfect Movie",
    'id': 499999
    },ignore_index=True)

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

# Fix director, cast, genre and keywords
metadata['director'] = metadata['crew'].apply(get_director)
features = ['cast', 'keywords', 'genres']
for feature in features:
    metadata[feature] = metadata[feature].apply(get_list)

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

# Save model for further use
np.savetxt("data/09_sim_artificial_movie.csv", cosine_sim2, fmt="%1.8f", delimiter=',')



# 10. BOOK DATASET -------------------------------------------------------------

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

# Save model for further use
np.savetxt("data/10_sim_books.csv", cosine_sim, fmt="%1.8f", delimiter=',')
