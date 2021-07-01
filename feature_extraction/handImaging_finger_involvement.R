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
OUTPUT_FILENAME <- "hand_imaging_finger_status.tsv"

############################
# Git Reference
############################
SCRIPT_PATH <- file.path(
    'feature_extraction', 
    'handImaging_finger_involvement.R')
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)

#' function to collapse finger list
#' @param data dataframe containing list
#' @return dataframe with collapsed list columns
collapse_list <- function(data, col){
    data$finger_nail <- purrr::map(data[[col]], function(x){
        if(length(x) == 0){
            return(NA_character_)
        }else{
            paste0(x, collapse = ", ")}}) %>% purrr::reduce(c)
    return(data)
}

#' Function to parse comma-separated finger 
#' location into columns
#' 
#' @param data dataframe
#' @return dataframe with finger as column
widen_finger_string_col <- function(data, col){
    purrr::map(FINGER_LIST, function(finger){
                     data %>%
                         dplyr::mutate(
                             !!sym(finger) := ifelse(
                                 !stringr::str_detect(
                                     string = !!sym(col),
                                     pattern = finger) | 
                                     is.na(!!sym(col)), 0, 1)
                         )}) %>% purrr::reduce(full_join) %>%
        dplyr::select(recordId, 
                      participantId, 
                      createdOn,
                      visit_num, 
                      all_of(FINGER_LIST))
}

main <- function(){
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
        collapse_list("finger_nail") %>%
        
        # parse each finger location to columns
        widen_finger_string_col("finger_nail") %>%
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        tibble::as_tibble() %>%
        
        # save to synapse
        save_to_synapse(output_filename = OUTPUT_FILENAME,
                        parent = PARENT_SYN_ID,
                        name = "get finger status",
                        executed = GIT_URL,
                        used = c(VISIT_SUMMARY,
                                 VISIT_REF_TBL_ID,
                                 PPACMAN_TBL_ID))
}

log_process(main(), SCRIPT_PATH)



