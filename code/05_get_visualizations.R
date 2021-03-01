library(magrittr)

save_plot <- function(df, out, ylim = NULL, hl = NULL, log = FALSE) {
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
  
  if (!is.null(ylim)) {
    p <- p + ggplot2::ylim(c(0, ylim))
  }
  if (!is.null(hl)) {
    
    df_hl <- df %>%
      readr::read_rds() %>%
      dplyr::arrange(-n) %>%
      dplyr::select(id, y = n) %>%
      tibble::rowid_to_column("t") %>%
      dplyr::filter(id == hl)
    
    p <- p + ggplot2::geom_point(data = df_hl, ggplot2::aes(x = t, y = y), color = "red")
  }
  if (log) {
    p <- p + ggplot2::scale_y_log10() + ggplot2::scale_x_log10()
    p <- p + ggplot2::xlab("Log Ranking") + ggplot2::ylab("Log Recommendations")
  }
  
  ggplot2::ggsave(out, p, width = 9, height = 9, units = "cm")
  
  p
}

# save_plot("data/01_rec_vanilla_overview.rds", "../text/figuras/01_rec_vanilla_overview.rds")

save_plot("data/16_rec_random.rds", "../text/figuras/1a_random.png")
save_plot("data/16_rec_random.rds", "../text/figuras/1b_random_log.png", log = TRUE)

save_plot("data/02_rec_vanilla.rds", "../text/figuras/2a_vanilla.png")
save_plot("data/02_rec_vanilla.rds", "../text/figuras/2b_vanilla_log.png", log = TRUE)

save_plot("data/04_rec_cutoff_low.rds", "../text/figuras/3a_cutoff_low.png", 3000)
save_plot("data/03_rec_cutoff_med.rds", "../text/figuras/3b_cutoff_med.png", 3000)
save_plot("data/05_rec_cutoff_high.rds", "../text/figuras/3c_cutoff_high.png", 3000)

save_plot("data/06_rec_cosine.rds", "../text/figuras/4a_cosine.png", 31000)
save_plot("data/07_rec_euclidean.rds", "../text/figuras/4b_euclidean.png", 31000)
save_plot("data/08_rec_manhattan.rds", "../text/figuras/4c_manhattan.png", 31000)

save_plot("data/11_rec_p.rds", "../text/figuras/5a_p.png", 200)
save_plot("data/12_rec_p10.rds", "../text/figuras/5b_p10.png", 200)
save_plot("data/13_rec_p100.rds", "../text/figuras/5c_p100.png", 200)

save_plot("data/09_rec_artificial_movie.rds", "../text/figuras/6a_artificial_movie.png", hl = 30689)
save_plot("data/14_rec_long.rds", "../text/figuras/6b_long.png")

save_plot("data/10_rec_books.rds", "../text/figuras/7a_books.png")
save_plot("data/15_rec_mimic.rds", "../text/figuras/7b_mimic.png")
