library(magrittr)

save_plot <- function(df, txt) {
  d <- df %>%
    readr::read_rds() %>%
    dplyr::arrange(-n) %>%
    dplyr::select(y = n) %>%
    tibble::rowid_to_column("t")
  
  p <- ggplot2::qplot(
    d$t, d$y, xlab = "Ranking",
    ylab = "Recommendations" #, main = txt
  ) +
    ggplot2::theme_minimal()
  
  file <- paste0("img/", fs::path_file(fs::path_ext_set(df, "pdf")))
  ggplot2::ggsave(file, p, width = 10, height = 10, units = "cm")
  
  p
}

save_plot("data/01_rec_vanilla_overview.rds", "Rec. based on movie summary")
save_plot("data/02_rec_vanilla.rds", "Vanilla (rec. based on metadata)")
save_plot("data/03_rec_cutoff_med.rds", "Cutoff n >= 5")
save_plot("data/04_rec_cutoff_low.rds", "Cutoff n >= 2")
save_plot("data/05_rec_cutoff_high.rds", "Cutoff n >= 8")
save_plot("data/06_rec_cosine.rds", "Cosine distance")
save_plot("data/07_rec_euclidean.rds", "Euclidean distance")
save_plot("data/08_rec_manhattan.rds", "Manhattan distance")
save_plot("data/09_rec_artificial_movie.rds", "Vanilla with artificial movie")
save_plot("data/10_rec_books.rds", "Movie dataset")
save_plot("data/11_rec_p.rds", "Random counts with P = P(Vanilla)")
save_plot("data/12_rec_p10.rds", "Random counts with P = P(Vanilla)*10")
save_plot("data/13_rec_p100.rds", "Random counts with P = P(Vanilla)*100")
save_plot("data/14_rec_long.rds", "Random counts long and narrow")
save_plot("data/15_rec_mimic.rds", "Random counts mimicking vanilla")
save_plot("data/16_rec_random.rds", "Random recommendations")
