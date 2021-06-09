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
source("curate_tables/table_utils.R")
synLogin()

MERGED_RESULT_PARENT_ID <- "syn25614358"
LEVEL_ORDER <- c(0,1,2,3,4)
PLAQUE_METRICS <- c("erythema", "induration", "scaling")


save_conf_mat_to_wiki <- function(data){
    data %>% 
        dplyr::select(annotator, synId, plot) %>%
        purrr::pwalk(function(annotator, synId, plot){
        output_filename <- "concordance_plot.jpeg"
        output_title <- glue::glue("Plaque Scoring Confusion Matrix - {annotator}")
        save_plot <- plot + 
            patchwork::plot_annotation(
                title = output_title,
                theme = theme(plot.title = element_text(size = 18, face = "bold"))) +
            ggsave(output_filename, width = 8, height = 8)
        entity <- synGet(synId)
        content <- paste0("${image?fileName=", 
                          output_filename, 
                          "&align=none&scale=50}")
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
            data %>% 
                dplyr::filter(!!sym(col_annot_string) != "Can't Tell") %>%
                dplyr::select(annot = !!sym(col_annot_string),
                              gs = !!sym(col_gs_string),
                              everything()) %>%
                dplyr::select(recordId, participantId, visit_num, annot, gs) %>%
                dplyr::mutate(annot = factor(annot, levels = level_order),
                              gs = factor(gs, levels = level_order)) %>%
                tibble::as_tibble() %>%
                yardstick::conf_mat(annot,
                                    gs,
                                    c("Annotation", 
                                      "Gold-Standard")) %>%
                tune::autoplot(type = "heatmap") +
                labs(title = str_to_title(metric)) +
                theme(plot.title = element_text(size = 12))}) %>% 
        patchwork::wrap_plots(ncol = 1)
}

main <- function(){
    synGetChildren(MERGED_RESULT_PARENT_ID)$asList() %>% 
        purrr::map(function(entity){
            synGet(entity$id)$path %>% fread() %>%
                dplyr::mutate(synId = entity$id)}) %>% 
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