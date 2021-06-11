figure <- purrr::map(unique(run_scoring$assessment), function(a){
    plot <- run_scoring %>% 
        dplyr::filter(assessment == a) %>%
        ggplot(aes(x = reorder(annotator, cc_corr), y = cc_corr)) +
        geom_segment( aes(x=reorder(annotator, cc_corr), 
                          xend=annotator, 
                          y=0, 
                          yend=cc_corr), 
                      color="skyblue3",
                      size = 2) +
        geom_point( color="orange1", size=15) +
        geom_text(aes(label = cc_corr), fontface = "bold") +
        coord_flip() +
        theme_light() +
        labs(x = "",
             y = "Concordance Scores",
             title = glue::glue("{a}")) +
        theme(
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(size = 25, face = "bold"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 15, face = "bold"),
            axis.text.x = element_blank(),
            axis.title.x = element_text(size = 15, face = "bold")
        ) 
}) %>% wrap_plots(., ncol = 3) + 
    plot_annotation(title = "Concordance Score Leaderboard",
                    subtitle = "Assessed using Lin's Concordance Correlation",
                    theme = theme(plot.title = element_text(size = 25, face = "bold"),
                                  plot.subtitle = element_text(size = 18)))