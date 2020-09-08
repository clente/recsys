library(magrittr)

# Get most recommended
movies <- "data/recommendations2.rds" %>%
  readr::read_rds() %>%
  dplyr::arrange(-n) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::pull(title)

# Read original datasets
credits <- readr::read_csv("data-raw/credits.csv")
keywords <- readr::read_csv("data-raw/keywords.csv")
metadata <- readr::read_csv("data-raw/movies_metadata.csv")

# Get movie IDs (this shouldn't have to happen)
ids <- metadata %>%
  dplyr::filter(title %in% movies) %>%
  dplyr::pull(id)

# "[{'id': 18, 'name': 'Drama'}, {'id': 35, 'name': 'Comedy'}]" 
metadata %>%
  dplyr::filter(id %in% ids) %>%
  dplyr::pull(genres)

# "[{'cast_id': 1010, 'character': 'Frédéric', 'credit_id': '52fe497dc3a36847f819cdf7', 'gender': 0, 'id': 228720, 'name': 'Yan Tassin', 'order': 8, 'profile_path': '/kKstl34DegL98G5JBQ8e7f1tsfu.jpg'}, {'cast_id': 11, 'character': '', 'credit_id': '56941ec09251414b6e000067', 'gender': 1, 'id': 121786, 'name': 'Ryō', 'order': 4, 'profile_path': '/oQ0o73MGCYs8jvoOcGBJniJrxWl.jpg'}, {'cast_id': 13, 'character': 'Nurse', 'credit_id': '56fce0dd9251415f96001b84', 'gender': 1, 'id': 1525065, 'name': 'Elizabeth Pan', 'order': 13, 'profile_path': '/7jzbZ65CTBko7o1f8BLXmyToqN9.jpg'}, {'cast_id': 1009, 'character': \"Yves, l'entraîneur de foot\", 'credit_id': '52fe497dc3a36847f819cdf3', 'gender': 2, 'id': 228719, 'name': 'Laurent Capelluto', 'order': 7, 'profile_path': '/fE6HK47D9rLnHwspIt27aCcFhz6.jpg'}, {'cast_id': 5, 'character': 'Hugo', 'credit_id': '52fe4cc1c3a36847f823d633', 'gender': 0, 'id': 129765, 'name': 'Jonathan Cohen', 'order': 2, 'profile_path': '/yKK4PD6P7UmQ18du9hEckgx8D0U.jpg'}, {'cast_id': 15, 'character': 'Sophie', 'credit_id': '570ee45ec3a3685370000fd1', 'gender': 1, 'id': 49, 'name': 'Maria Bello', 'order': 1, 'profile_path': '/tFkbad0JoWvYc6XYBITv6EfeLwR.jpg'}, {'cast_id': 10, 'character': 'Thomás (age 11)', 'credit_id': '52fe4678c3a36847f8100d0d', 'gender': 0, 'id': 130306, 'name': 'Lucas Cotrim', 'order': 6, 'profile_path': None}, {'cast_id': 1029, 'character': 'Mme Legrand', 'credit_id': '577c6b17c3a3683f57001198', 'gender': 0, 'id': 1645937, 'name': 'Catherine Beau', 'order': 16, 'profile_path': '/iRmHyHDmoGg5YNSuOtqQBKj2jRe.jpg'}, {'cast_id': 21, 'character': 'Esther', 'credit_id': '579dfcb1c3a3684b8a0018a1', 'gender': 1, 'id': 1302131, 'name': 'Lotta Losten', 'order': 9, 'profile_path': '/oxlgUrg78Aqg55Ck4a4uGYb2R1B.jpg'}, {'cast_id': 3, 'character': 'Marcio', 'credit_id': '52fe4cbfc3a36847f823d14f', 'gender': 0, 'id': 1149677, 'name': 'Breno Viola', 'order': 1, 'profile_path': None}, {'cast_id': 9, 'character': 'Kiran', 'credit_id': '591567edc3a36842d601af05', 'gender': 0, 'id': 1815255, 'name': 'Sukra Raj Rokaya', 'order': 2, 'profile_path': None}, {'cast_id': 32, 'character': 'Monk', 'credit_id': '5987e56f9251415244015b93', 'gender': 0, 'id': 930345, 'name': 'Pistol Takehara', 'order': 8, 'profile_path': '/9FAHgWdf5yQrE4wExS78ENoWtp.jpg'}, {'cast_id': 1, 'character': 'Mitchell', 'credit_id': '52fe44cb9251416c9101d22d', 'gender': 2, 'id': 10671, 'name': 'Joe Don Baker', 'order': 0, 'profile_path': '/bLvWzzb9SyoyjwPpj5pgzXb9tuC.jpg'}, {'cast_id': 5, 'character': 'Greta Adams', 'credit_id': '52fe44cb9251416c9101d239', 'gender': 1, 'id': 20928, 'name': 'Linda Evans', 'order': 3, 'profile_path': '/hDAUmTGeD8ZP9zW3DcBm9FQwlip.jpg'}, {'cast_id': 9, 'character': '', 'credit_id': '5691d954c3a3684cf8000a51', 'gender': 2, 'id': 93892, 'name': 'Masatō Ibu', 'order': 6, 'profile_path': '/oVy1sMt7DVttkG5Gpxcv6ullVU9.jpg'}, {'cast_id': 10, 'character': 'Patricia', 'credit_id': '52fe4cbfc3a36847f823d16b', 'gender': 0, 'id': 1149680, 'name': 'Juliana Didone', 'order': 8, 'profile_path': None}, {'cast_id': 22, 'character': 'Young Rebecca', 'credit_id': '58704815c3a3684f12001618', 'gender': 1, 'id': 1493969, 'name': 'Amiah Miller', 'order': 11, 'profile_path': '/5hnbUi2K9Xu8QoDGnSPQDot3G06.jpg'}, {'cast_id': 1027, 'character': 'Florence', 'credit_id': '577c69e1925141577e000f63', 'gender': 0, 'id': 1645932, 'name': 'Edith Proust', 'order': 14, 'profile_path': None}, {'cast_id': 13, 'character': 'Pai de Stalone', 'credit_id': '581a1812c3a36853a5000a5f', 'gender': 0, 'id': 1072146, 'name': 'Pedro Urizzi', 'order': 11, 'profile_path': '/x5w14XTFHAOyDGiTQ2T4bd6f1FJ.jpg'}, {'cast_id': 8, 'character': 'Pedro', 'credit_id': '52fe4678c3a36847f8100d05', 'gender': 0, 'id': 105738, 'name': 'Jean Pierre Noher', 'order': 4, 'profile_path': '/iCaFtj2sFjK2gYZryZK0uwY6RSE.jpg'}]"
credits %>%
  dplyr::filter(id %in% ids) %>%
  dplyr::pull(cast) %>%
  stringr::str_split("\\}, \\{") %>%
  purrr::flatten_chr() %>%
  stringr::str_remove("\\[\\{") %>%
  stringr::str_remove("\\}\\]") %>%
  base::sample(20) %>%
  stringr::str_c(collapse = "}, {") %>%
  stringr::str_c("[{", ., "}]")

# "[{'credit_id': '55890341c3a3681dca0016f9', 'department': 'Directing', 'gender': 2, 'id': 1302082, 'job': 'Director', 'name': 'David F. Sandberg', 'profile_path': '/10p2i4OW0JWAbnOb7yMLUEGdSqM.jpg'}]"
credits %>%
  dplyr::filter(id %in% ids) %>%
  dplyr::pull(crew)

# "[{'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}, {'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}]"
keywords %>%
  dplyr::filter(id %in% ids) %>%
  dplyr::pull(keywords)

my_credits <- credits %>%
  dplyr::add_row(
    cast = "[{'cast_id': 1010, 'character': 'Frédéric', 'credit_id': '52fe497dc3a36847f819cdf7', 'gender': 0, 'id': 228720, 'name': 'Yan Tassin', 'order': 8, 'profile_path': '/kKstl34DegL98G5JBQ8e7f1tsfu.jpg'}, {'cast_id': 11, 'character': '', 'credit_id': '56941ec09251414b6e000067', 'gender': 1, 'id': 121786, 'name': 'Ryō', 'order': 4, 'profile_path': '/oQ0o73MGCYs8jvoOcGBJniJrxWl.jpg'}, {'cast_id': 13, 'character': 'Nurse', 'credit_id': '56fce0dd9251415f96001b84', 'gender': 1, 'id': 1525065, 'name': 'Elizabeth Pan', 'order': 13, 'profile_path': '/7jzbZ65CTBko7o1f8BLXmyToqN9.jpg'}, {'cast_id': 1009, 'character': \"Yves, l'entraîneur de foot\", 'credit_id': '52fe497dc3a36847f819cdf3', 'gender': 2, 'id': 228719, 'name': 'Laurent Capelluto', 'order': 7, 'profile_path': '/fE6HK47D9rLnHwspIt27aCcFhz6.jpg'}, {'cast_id': 5, 'character': 'Hugo', 'credit_id': '52fe4cc1c3a36847f823d633', 'gender': 0, 'id': 129765, 'name': 'Jonathan Cohen', 'order': 2, 'profile_path': '/yKK4PD6P7UmQ18du9hEckgx8D0U.jpg'}, {'cast_id': 15, 'character': 'Sophie', 'credit_id': '570ee45ec3a3685370000fd1', 'gender': 1, 'id': 49, 'name': 'Maria Bello', 'order': 1, 'profile_path': '/tFkbad0JoWvYc6XYBITv6EfeLwR.jpg'}, {'cast_id': 10, 'character': 'Thomás (age 11)', 'credit_id': '52fe4678c3a36847f8100d0d', 'gender': 0, 'id': 130306, 'name': 'Lucas Cotrim', 'order': 6, 'profile_path': None}, {'cast_id': 1029, 'character': 'Mme Legrand', 'credit_id': '577c6b17c3a3683f57001198', 'gender': 0, 'id': 1645937, 'name': 'Catherine Beau', 'order': 16, 'profile_path': '/iRmHyHDmoGg5YNSuOtqQBKj2jRe.jpg'}, {'cast_id': 21, 'character': 'Esther', 'credit_id': '579dfcb1c3a3684b8a0018a1', 'gender': 1, 'id': 1302131, 'name': 'Lotta Losten', 'order': 9, 'profile_path': '/oxlgUrg78Aqg55Ck4a4uGYb2R1B.jpg'}, {'cast_id': 3, 'character': 'Marcio', 'credit_id': '52fe4cbfc3a36847f823d14f', 'gender': 0, 'id': 1149677, 'name': 'Breno Viola', 'order': 1, 'profile_path': None}, {'cast_id': 9, 'character': 'Kiran', 'credit_id': '591567edc3a36842d601af05', 'gender': 0, 'id': 1815255, 'name': 'Sukra Raj Rokaya', 'order': 2, 'profile_path': None}, {'cast_id': 32, 'character': 'Monk', 'credit_id': '5987e56f9251415244015b93', 'gender': 0, 'id': 930345, 'name': 'Pistol Takehara', 'order': 8, 'profile_path': '/9FAHgWdf5yQrE4wExS78ENoWtp.jpg'}, {'cast_id': 1, 'character': 'Mitchell', 'credit_id': '52fe44cb9251416c9101d22d', 'gender': 2, 'id': 10671, 'name': 'Joe Don Baker', 'order': 0, 'profile_path': '/bLvWzzb9SyoyjwPpj5pgzXb9tuC.jpg'}, {'cast_id': 5, 'character': 'Greta Adams', 'credit_id': '52fe44cb9251416c9101d239', 'gender': 1, 'id': 20928, 'name': 'Linda Evans', 'order': 3, 'profile_path': '/hDAUmTGeD8ZP9zW3DcBm9FQwlip.jpg'}, {'cast_id': 9, 'character': '', 'credit_id': '5691d954c3a3684cf8000a51', 'gender': 2, 'id': 93892, 'name': 'Masatō Ibu', 'order': 6, 'profile_path': '/oVy1sMt7DVttkG5Gpxcv6ullVU9.jpg'}, {'cast_id': 10, 'character': 'Patricia', 'credit_id': '52fe4cbfc3a36847f823d16b', 'gender': 0, 'id': 1149680, 'name': 'Juliana Didone', 'order': 8, 'profile_path': None}, {'cast_id': 22, 'character': 'Young Rebecca', 'credit_id': '58704815c3a3684f12001618', 'gender': 1, 'id': 1493969, 'name': 'Amiah Miller', 'order': 11, 'profile_path': '/5hnbUi2K9Xu8QoDGnSPQDot3G06.jpg'}, {'cast_id': 1027, 'character': 'Florence', 'credit_id': '577c69e1925141577e000f63', 'gender': 0, 'id': 1645932, 'name': 'Edith Proust', 'order': 14, 'profile_path': None}, {'cast_id': 13, 'character': 'Pai de Stalone', 'credit_id': '581a1812c3a36853a5000a5f', 'gender': 0, 'id': 1072146, 'name': 'Pedro Urizzi', 'order': 11, 'profile_path': '/x5w14XTFHAOyDGiTQ2T4bd6f1FJ.jpg'}, {'cast_id': 8, 'character': 'Pedro', 'credit_id': '52fe4678c3a36847f8100d05', 'gender': 0, 'id': 105738, 'name': 'Jean Pierre Noher', 'order': 4, 'profile_path': '/iCaFtj2sFjK2gYZryZK0uwY6RSE.jpg'}]",
    crew = "[{'credit_id': '55890341c3a3681dca0016f9', 'department': 'Directing', 'gender': 2, 'id': 1302082, 'job': 'Director', 'name': 'David F. Sandberg', 'profile_path': '/10p2i4OW0JWAbnOb7yMLUEGdSqM.jpg'}]",
    id = 499999
  ) %>%
  readr::write_csv("data-raw/my_credits.csv")

my_keywords <- keywords %>%
  dplyr::add_row(
    id = 499999,
    keywords = "[{'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}, {'id': 9840, 'name': 'romance'}, {'id': 10267, 'name': 'comedy'}]"
  ) %>%
  readr::write_csv("data-raw/my_keywords.csv")

my_metadata <- metadata %>%
  dplyr::add_row(
    genres = "[{'id': 18, 'name': 'Drama'}, {'id': 35, 'name': 'Comedy'}]",
    id = 499999,
    title = "My Perfect Movie"
  ) %>%
  readr::write_csv("data-raw/my_movies_metadata.csv")
