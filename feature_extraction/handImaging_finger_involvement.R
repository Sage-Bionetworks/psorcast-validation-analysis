rm(list=ls())
gc()

# import libraries
library(synapser)
library(data.table)
library(tidyverse)
library(githubr)
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')
synLogin()

############################
# Global Vars
############################
PARENT_SYN_ID <- "syn22342373"
ERROR_LOG_SYN_ID <- "syn25832341"
VISIT_REF_TBL_ID <- "syn25825626"
PPACMAN_TBL_ID <- "syn22337133"
VISIT_SUMMARY <- "syn25832372"
FINGER_LIST <- c("L1", "L2", "L3", "L4", "L5",
                 "R1", "R2", "R3", "R4", "R5")

collapse_finger_list <- function(data){
    data$finger_nail <- purrr::map(data$finger_nail, function(x){
        if(length(x) == 0){
            return(NA_character_)
        }else{
            paste0(x, collapse = ", ")}}) %>% purrr::reduce(c)
    return(data)
}

parse_finger_location <- function(data){
    purrr::map(c("L1", "L2", "L3", "L4", "L5",
                 "R1", "R2", "R3", "R4", "R5"), function(finger){
                     data %>%
                         dplyr::mutate(
                             !!sym(finger) := ifelse(
                                 !stringr::str_detect(
                                     string = finger_nail,
                                     pattern = finger) | 
                                     is.na(finger_nail), 0, 1)
                         )}) %>% purrr::reduce(full_join) %>%
        dplyr::select(recordId, 
                      participantId, 
                      createdOn,
                      visit_num, 
                      all_of(FINGER_LIST))
}

#' get visit reference and curated ppacman table
visit_ref <- synGet(VISIT_REF_TBL_ID)$path %>% fread()
ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread()

#' read summary data
data <- synGet(VISIT_SUMMARY)$path %>% 
    fread() %>%
    
    # get hand imaging from summary
    dplyr::filter(source == "HandImaging-Curated") %>%
    dplyr::select(recordId, participantId, createdOn) %>%
    
    # join with ppacman
    join_with_ppacman(visit_ref, ppacman) %>% 
    
    # hard code some finger nail involvement
    dplyr::mutate(
        finger_nail = ifelse(
            finger_nail == 
                "onycholysis L thumb", "L5", finger_nail)) %>% 
    dplyr::mutate(
        finger_nail = ifelse(
            finger_nail == 
                "L1 (onycholysis), L2, 3 (pitting)", "L1, L2, L3", finger_nail)) %>% 
    
    # collapse cleaned finger nail data
    dplyr::mutate(
        finger_nail = stringr::str_extract_all(
            finger_nail, (FINGER_LIST %>% paste0(collapse = "|")))) %>%
    collapse_finger_list() %>%
    
    # parse each finger location to columns
    parse_finger_location() %>%
    tibble::as_tibble()



