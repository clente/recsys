import numpy as np
import pandas as pd
import tensorflow as tf
import tensorflow_datasets as tfds
import tensorflow_recommenders as tfrs

file_predictions = "data-raw/predictions3.csv"
dir_model = "data-raw/1m_model3"

### Chunk 1 --------------------------------------------------------------------

users_movies = pd.read_csv(
  'data-raw/users_movies.csv',
  dtype = {'user_id': str,'movie_title': str})

model = tf.saved_model.load(dir_model)

predictions = []
for i in range(0, 11):
  st = i * 500_000
  nd = (i + 1) * 500_000
  print(st)
  chunk = users_movies.iloc[range(st, nd)].to_dict('list')
  df = pd.DataFrame(model(chunk).numpy())
  df.to_csv(file_predictions, mode='a', header=None, index=None)

5_500_000/500_000

### Chunk 2 --------------------------------------------------------------------

users_movies = pd.read_csv(
  'data-raw/users_movies.csv',
  dtype = {'user_id': str,'movie_title': str})

model = tf.saved_model.load(dir_model)

predictions = []
for i in range(11, 23):
  st = i * 500_000
  nd = (i + 1) * 500_000
  print(st)
  chunk = users_movies.iloc[range(st, nd)].to_dict('list')
  df = pd.DataFrame(model(chunk).numpy())
  df.to_csv(file_predictions, mode='a', header=None, index=None)

11500000/500_000

### Chunk 3 --------------------------------------------------------------------

users_movies = pd.read_csv(
  'data-raw/users_movies.csv',
  dtype = {'user_id': str,'movie_title': str})

model = tf.saved_model.load(dir_model)

predictions = []
for i in range(23, 36):
  st = i * 500_000
  nd = (i + 1) * 500_000
  print(st)
  chunk = users_movies.iloc[range(st, nd)].to_dict('list')
  df = pd.DataFrame(model(chunk).numpy())
  df.to_csv(file_predictions, mode='a', header=None, index=None)

18000000/500_000

### Chunk 4 --------------------------------------------------------------------

users_movies = pd.read_csv(
  'data-raw/users_movies.csv',
  dtype = {'user_id': str,'movie_title': str})

model = tf.saved_model.load(dir_model)

predictions = []
for i in range(36, 45):
  st = i * 500_000
  nd = (i + 1) * 500_000
  print(st)
  chunk = users_movies.iloc[range(st, nd)].to_dict('list')
  df = pd.DataFrame(model(chunk).numpy())
  df.to_csv(file_predictions, mode='a', header=None, index=None)

chunk = users_movies.iloc[22000000:].to_dict('list')
df = pd.DataFrame(model(chunk).numpy())
df.to_csv(file_predictions, mode='a', header=None, index=None)
