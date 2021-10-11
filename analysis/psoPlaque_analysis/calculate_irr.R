#######################################################################
# This code is used to calculate inter-rater reliability (Kappa2)
# Across Psorisasis Plaque scoring metrics
# @maintainer: aryton.tediarjo@sagebase.org
######################################################################

#' import library
library(synapser)
library(tidyverse)
library(data.table)
library(githubr)
library(irr)

#' log in to synapse
synLogin()

#' merged annotations parent ids
MERGED_PARENT_ID <- "syn25614358"

#' Function for getting kappa score relative to gold standard
#' accross plaque metrics scores
#' 
#' @param data data of row-binded merged annotations
#' @return data with kappa score for each group and plaque metrics
kappa_relative_to_gs <- function(data){
    data <- data %>%
        tidyr::pivot_wider(names_from = gs_status,
                           values_from = value,
                           id_cols = c("annotator","recordId", 
                                       "is_gold_standard"))
    is_gs <- data %>%
        dplyr::filter(is_gold_standard == TRUE) %>%
        dplyr::select(gs,annot) %>%
        dplyr::filter(annot != "-1") %>%
        irr::kappa2("equal") %>% 
        .$value
    
    not_gs <- data %>%
        dplyr::filter(is_gold_standard != TRUE) %>%
        dplyr::select(gs,annot) %>%
        dplyr::filter(annot != "-1") %>%
        irr::kappa2("equal") %>% 
        .$value
    
    tibble(annot_status = c("gs","annotation"),
           kappa_value = c(is_gs, not_gs))
}

#' Function to plot kappa comparison 
#' across plaque status scores compared 
#' using shiny app annotators and in-clinic score
#' 
#' @data data containing kappa score
#' @return ggplot object 
plot_kappa <- function(data){
    data %>% 
        ggplot(aes(x = status, y = kappa_value, 
                   fill = annot_status)) +
        geom_bar(stat = "identity", 
                 width = 0.5,
                 position = position_dodge()) +
        geom_text(aes(label = kappa_value),
                  vjust=-0.5,
                  position = position_dodge(0.5), size=3.5) +
        scale_fill_brewer(palette="Paired") +
        labs(title = "Agreement of App Annotation Relative to Physician In-Clinic Assessment",
             subtitle = "Assessed based on Kappa-Score of Trainees vs Gold-Standard",
             y = "Agreement (Kappa)") +
        theme_minimal()
}



main <- function(){
    #' get child of folder Id
    data <- synGetChildren(MERGED_PARENT_ID)$asList() %>%
        purrr::map(function(x){
            # get annotator
            annotator <- stringr::str_split(x$name, "_") %>% 
                unlist() %>% 
                .[[1]]
            
            # get file
            data <- synGet(x$id)$path %>% 
                fread() %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::select(-annotator) %>% 
                dplyr::mutate(annotator = annotator)}) %>%
        
        # bind each retrieved dataframe
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::select(annotator, 
                      recordId,
                      annot_erythema, 
                      gs_erythema,
                      annot_scaling,
                      gs_scaling,
                      annot_induration,
                      gs_induration,
                      annot_pga,
                      gs_pga) %>%
        dplyr::group_by(
            annotator, recordId) %>%
        
        # map can't tell as -1
        dplyr::mutate_all(~ifelse(.x == "Can't Tell", "-1", .x)) %>%
        dplyr::mutate_all(as.numeric) %>%
        dplyr::ungroup()  %>% 
        dplyr::mutate(is_gold_standard = case_when(
            annotator == "rhaberman" ~ TRUE,
            annotator == "castir06"~ TRUE,
            annotator == "lmpch" ~ TRUE,
            annotator == "mreolla" ~ TRUE,
            TRUE ~ FALSE)) %>%
        
        # remove my own annotation
        dplyr::filter(annotator != "atediarjo")
    
    
    plot <- data %>% 
        
        # pivot data
        pivot_longer(!all_of(c("annotator","recordId", "is_gold_standard"))) %>%
        
        # separate columns to get unique column string
        tidyr::separate(name, "_", into = c("gs_status", "status")) %>% 
        
        # nest by plaque status
        dplyr::group_by(status) %>% 
        tidyr::nest() %>% 
        
        # mutate each nested data
        dplyr::mutate(kappa_score = purrr::map(data, kappa_relative_to_gs)) %>%
        tidyr::unnest(kappa_score) %>% 
        dplyr::mutate(kappa_value = sprintf("%.2f", kappa_value)) %>%
        
        # plot kappa score using bar plot
        plot_kappa()
    
}

