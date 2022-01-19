library(synapser)
library(knit2synapse)
library(tidyverse)
library(dplyr)
library(githubr)
library(ggpval)
library(ggExtra)
library(ggpubr)
library(data.table)
library(DescTools)

synLogin()
MERGED_FEATURES <- "syn26841919"
PARENT_ID <- "syn26840743"
OUTPUT_FILE_REF <- list(
    draw = list(plot = NULL, output_file = "Figure_1B.png"),
    tjc = list(plot = NULL, output_file = "Figure_2B.png")
)

plot_bland_altman <- function(data, 
                              dig_measure, 
                              gs_measure,
                              label_x_coord,
                              label_y_coord){
    # get correlation values
    cc.corr <- DescTools::CCC(
        data[[dig_measure]], 
        data[[gs_measure]], 
        ci = "z-transform", 
        conf.level = 0.95, 
        na.rm = FALSE) %>% 
        .$rho.c
    
    label <- glue::glue("CCC = {ccc} ({lci}, {uci})",
                        lci = round(cc.corr$lwr.ci, 2),
                        uci = round(cc.corr$upr.ci, 2),
                        ccc = round(cc.corr$est, 2))
    
    bland.altman <- blandr::blandr.draw(
        data[[dig_measure]], 
        data[[gs_measure]], 
        ciDisplay = FALSE,
        point_size = 3) +
        geom_point(color = "dark green") +
        annotate("text", 
                 label = label, 
                 x = label_x_coord, 
                 y = label_y_coord, 
                 size = 4)
    return(bland.altman)
}



data <-  fread(synGet(MERGED_FEATURES)$path)
OUTPUT_FILE_REF$tjc$plot <- plot_bland_altman(
    data %>%
        tidyr::drop_na(dig_jc_counts, gs_jc_counts),
    dig_measure = "dig_jc_counts",
    gs_measure = "gs_jc_counts",
    label_x_coord = 15,
    label_y_coord = 40) +
    scale_y_continuous(name = "TJC Difference", 
                       expand=c(0,0), 
                       limits=c(-50,50)) +
    labs(title = "Patient vs Physician Tender Joint Count") + 
    theme_minimal() + 
    theme(plot.title = element_text(
        family = "sans", vjust = -1, 
        size = 20, margin=margin(0,0,60,0)))

OUTPUT_FILE_REF$draw$plot <- plot_bland_altman(
    data %>% 
        dplyr::mutate(dig_bsa = dig_bsa * 100) %>%
        drop_na(c(dig_bsa, gs_bsa)),
    dig_measure = "dig_bsa",
    gs_measure = "gs_bsa",
    label_x_coord = 15,
    label_y_coord = 20) +
    scale_y_continuous(name = "BSA Difference", 
                       expand=c(0,0), 
                       limits=c(-25,25)) +
    labs(title = "Patient Digital vs Physician Body Surface Area") + 
    theme_minimal() + 
    theme(plot.title = element_text(
        family = "sans", vjust = -1, 
        size = 20, margin=margin(0,0,60,0)))


purrr::map(OUTPUT_FILE_REF, function(content){
    ggsave(content$output_file, content$plot)
    file = synapser::File(content$output_file, parent = PARENT_ID)
    synStore(file)
    unlink(content$output_file)
})
