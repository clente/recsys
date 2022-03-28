import tensorflow as tf
import tensorflow_datasets as tfds
import tensorflow_recommenders as tfrs
import numpy as np

ratings = tfds.load("movielens/1m-ratings", split="train", shuffle_files=True)
movies = tfds.load("movielens/1m-movies", split="train", shuffle_files=True)

ratings = ratings.map(lambda x: {
    "movie_id": x["movie_id"],
    "user_id": x["user_id"],
    "user_rating": x["user_rating"],
    # "user_gender": int(str(x["user_gender"])),
    "user_zip_code": x["user_zip_code"],
    "user_occupation_text": x["user_occupation_text"],
    # "bucketized_user_age": int(str(x["bucketized_user_age"]))
})

tf.random.set_seed(42)
shuffled = ratings.shuffle(1_000_000, seed=42, reshuffle_each_iteration=False)

train = shuffled.take(800_000)
test = shuffled.skip(800_000).take(200_000)

feature_names = ["movie_id", "user_id", "user_zip_code", "user_occupation_text"]

vocabularies = {}

for feature_name in feature_names:
    vocab = ratings.batch(10_000_000).map(lambda x: x[feature_name])
    vocabularies[feature_name] = np.unique(np.concatenate(list(vocab)))

class DCN(tfrs.Model):
    def __init__(self, use_cross_layer, deep_layer_sizes, projection_dim=None):
        super().__init__()
        self.embedding_dimension = 32
        str_features = ["movie_id", "user_id", "user_zip_code",
                        "user_occupation_text"]
        # int_features = ["user_gender", "bucketized_user_age"]
        self._all_features = str_features # + int_features
        self._embeddings = {}
        # Compute embeddings for string features.
        for feature_name in str_features:
            vocabulary = vocabularies[feature_name]
            self._embeddings[feature_name] = tf.keras.Sequential(
                [tf.keras.layers.StringLookup(
                    vocabulary=vocabulary, mask_token=None),
                tf.keras.layers.Embedding(len(vocabulary) + 1, self.embedding_dimension)
        ])
        # # Compute embeddings for int features.
        # for feature_name in int_features:
        #   vocabulary = vocabularies[feature_name]
        #   self._embeddings[feature_name] = tf.keras.Sequential(
        #       [tf.keras.layers.IntegerLookup(
        #           vocabulary=vocabulary, mask_value=None),
        #        tf.keras.layers.Embedding(len(vocabulary) + 1,
        #                                  self.embedding_dimension)
        # ])
        if use_cross_layer:
            self._cross_layer = tfrs.layers.dcn.Cross(
                projection_dim=projection_dim,
                kernel_initializer="glorot_uniform")
        else:
            self._cross_layer = None
        self._deep_layers = [tf.keras.layers.Dense(layer_size, activation="relu")
            for layer_size in deep_layer_sizes]
        self._logit_layer = tf.keras.layers.Dense(1)
        self.task = tfrs.tasks.Ranking(
            loss=tf.keras.losses.MeanSquaredError(),
            metrics=[tf.keras.metrics.RootMeanSquaredError("RMSE")]
        )
    def call(self, features):
        # Concatenate embeddings
        embeddings = []
        for feature_name in self._all_features:
            embedding_fn = self._embeddings[feature_name]
            embeddings.append(embedding_fn(features[feature_name]))
        x = tf.concat(embeddings, axis=1)
        # Build Cross Network
        if self._cross_layer is not None:
            x = self._cross_layer(x)
        # Build Deep Network
        for deep_layer in self._deep_layers:
            x = deep_layer(x)
        return self._logit_layer(x)
    def compute_loss(self, features, training=False):
        labels = features.pop("user_rating")
        scores = self(features)
        return self.task(
            labels=labels,
            predictions=scores,
        )

cached_train = train.shuffle(1_000_000).batch(8192).cache()
cached_test = test.batch(4096).cache()

epochs = 8
learning_rate = 0.01

# def run_models(use_cross_layer, deep_layer_sizes, projection_dim=None, num_runs=5):
#     models = []
#     rmses = []
#     for i in range(num_runs):
#         model = DCN(use_cross_layer=use_cross_layer,
#                     deep_layer_sizes=deep_layer_sizes,
#                     projection_dim=projection_dim)
#         model.compile(optimizer=tf.keras.optimizers.Adam(learning_rate))
#         models.append(model)
#         model.fit(cached_train, epochs=epochs, verbose=False)
#         metrics = model.evaluate(cached_test, return_dict=True)
#         rmses.append(metrics["RMSE"])
#     mean, stdv = np.average(rmses), np.std(rmses)
#     return {"model": models, "mean": mean, "stdv": stdv}

# dcn_lr_result = run_models(use_cross_layer=True,
#                            projection_dim=20,
#                            deep_layer_sizes=[192, 192])

# print("DCN (low-rank) RMSE mean: {:.4f}, stdv: {:.4f}".format(
#     dcn_lr_result["mean"], dcn_lr_result["stdv"]))

model = DCN(use_cross_layer=True, deep_layer_sizes=[192, 192], projection_dim=20)
model.compile(optimizer=tf.keras.optimizers.Adam(learning_rate))
model.fit(cached_train, epochs=epochs, verbose=False)

tf.saved_model.save(model, "cdn")

loaded = tf.saved_model.load("cdn")

loaded({
    "movie_id": np.array(["362"]),
    "user_id": np.array(["42"]),
    "user_zip_code": np.array(["62704"]),
    "user_occupation_text": ["other/not specified"],
}).numpy()
# array([[3.338933]], dtype=float32)
