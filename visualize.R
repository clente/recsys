library(magrittr)

view <- function(df, txt) {
  d <- df %>%
    readr::read_rds() %>%
    dplyr::arrange(-n) %>%
    dplyr::select(y = n) %>%
    tibble::rowid_to_column("t")
  ggplot2::qplot(d$t, d$y, xlab = "Ranking Filme", ylab = "No. Recomendações", main = txt)
}

view("data/recommendations1.rds", "Vanilla Usando Descrição do Filme")
view("data/recommendations2.rds", "Vanilla")
view("data/my_recommendations.rds", "Vanilla + Filme Artificial")
view("data/rand_recommendations1.rds", "Bernoulli com P = Vanilla")
view("data/rand_recommendations2.rds", "Bernoulli com P = Vanilla*10")
view("data/rand_recommendations3.rds", "Bernoulli com P = Vanilla*100")
view("data/rand_recommendations5.rds", "Bernoulli Longo e Estreito")
view("data/recommendations3.rds", "Vanilla mantendo n >= 5")
view("data/recommendations4.rds", "Vanilla mantendo n >= 2")
view("data/recommendations5.rds", "Vanilla mantendo n >= 8")
view("data/rand_recommendations7.rds", "Bernoulli com Perfil do Vanilla")

view("data/recommendations_books.rds", "Dataset de livros")

view("data/bad_recommendations.rds", "Escolhendo recomendações ao acaso")
l
view("data/recommendations6.rds", "Vanilla Usando Distância de Cosseno")
view("data/recommendations7.rds", "Vanilla Usando Distância Euclidiana")
view("data/recommendations8.rds", "Vanilla Usando Distância de Manhattan")
