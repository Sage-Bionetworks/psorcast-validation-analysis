############################################
#' Script to generate confusion matrix for 
#' each annotator merged dataset
#' 
#' Author: Aryton Tediarjo, Meghasyam Tummalacherla
#' Maintainer: aryton.tediarjo@sagebase.org
#############################################
rm(list=ls())
gc()

library(synapser)
library(githubr)
library(data.table)
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
synLogin()

MERGED_RESULT_PARENT_ID <- "syn25614358"
LEVEL_ORDER <- c(0,1,2,3,4)
PLAQUE_METRICS <- c("erythema", "induration", "scaling", "pga")


compute_ccc <- function(data, truth, estimate){
    data %>% 
        yardstick::ccc(!!sym(truth), !!sym(estimate))
} 

save_conf_mat_to_wiki <- function(data){
    data %>% 
        dplyr::select(annotator, synId, plot) %>%
        purrr::pwalk(function(annotator, synId, plot){
        output_filename <- "concordance_plot.jpeg"
        output_title <- glue::glue("Plaque Scoring Confusion Matrix")
        output_subtitle <- glue::glue("\nAnnotator: {annotator}\nTimestamp: {timestamp}\n",
                                      annotator = annotator, 
                                      timestamp = as.character(lubridate::now()))
        save_plot <- plot + 
            patchwork::plot_annotation(
                title = output_title,
                subtitle = output_subtitle,
                theme = theme(
                    plot.title = element_text(size = 20),
                    plot.subtitle = element_text(size = 13, margin=margin(30,0,30,0))))
        ggsave(output_filename, save_plot,  width = 10, height = 10)
        entity <- synGet(synId)
        content <- paste0("${image?fileName=", 
                          output_filename, 
                          "&align=none&scale=70}")
        wiki = Wiki(title = "concordance matrix",
                    owner = entity,
                    markdown = content,
                    attachments = list(output_filename))
        synStore(wiki)
    })
} 

plot_confusion_matrix <- function(data, 
                                  metrics,
                                  level_order){
    metrics %>%
        purrr::map(function(metric){
            col_annot_string <- glue::glue("annot_{metric}")
            col_gs_string <- glue::glue("gs_{metric}")
            data <- data %>% 
                dplyr::filter(!!sym(col_annot_string) != "Can't Tell") %>% 
                dplyr::select(annot = !!sym(col_annot_string),
                              gs = !!sym(col_gs_string),
                              everything()) %>%
                dplyr::select(recordId, participantId, visit_num, annot, gs) %>%
                dplyr::mutate(annot = factor(annot, levels = level_order),
                              gs = factor(gs, levels = level_order)) %>%
                tibble::as_tibble()
            ccc_score <- data %>%
                dplyr::mutate(across(c("annot", "gs"), as.numeric)) %>%
                compute_ccc("gs", "annot") %>%
                .$.estimate %>%
                sprintf(fmt = '%#.3f') %>%
                glue::glue("Lin's CCC: {score}", score = .)
            data %>%
                yardstick::conf_mat(annot,
                                    gs,
                                    c("Annotation", 
                                      "Gold-Standard")) %>%
                tune::autoplot(type = "heatmap") +
                scale_fill_gradient(low = "white") +
                labs(title = str_to_title(metric),
                     subtitle = str_to_title(ccc_score)) +
                theme(plot.title = element_text(size = 16),
                      axis.title.x = element_text(size = 14),
                      axis.title.y = element_text(size = 14))}) %>% 
        patchwork::wrap_plots(ncol = 2, nrow = 2)
}

main <- function(){
    synGetChildren(MERGED_RESULT_PARENT_ID)$asList() %>% 
        purrr::map(function(entity){
            synGet(entity$id)$path %>% fread() %>%
                dplyr::mutate(synId = entity$id) %>%
                dplyr::mutate_all(as.character)}) %>% 
        purrr::reduce(bind_rows) %>%
        dplyr::group_by(annotator, synId) %>% 
        tidyr::nest() %>%
        dplyr::mutate(plot = purrr::map(
            data, plot_confusion_matrix, 
            metrics = PLAQUE_METRICS,
            level_order = LEVEL_ORDER)) %>%
        save_conf_mat_to_wiki()
}

main()