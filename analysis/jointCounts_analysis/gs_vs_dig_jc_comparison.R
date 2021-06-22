############################################################
# This script will be used for summarizing reported each 
# joint counts for PSA users
# @author: aryton.tediarjo@sagebase.org
############################################################
rm(list=ls())
gc()

# import libraries
library(synapser)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(jsonlite)
library(githubr)
synLogin()
source("utils/feature_extraction_utils.R")


############################
# Global Vars
############################
PARENT_SYN_ID <- "syn25704998"
GS_JOINT_COUNT <- "syn22281781"
DIG_JOINT_COUNT <- "syn22281786"
PPACMAN_TBL_ID <- "syn22337133"
VISIT_REF_ID <- "syn25825626"
FILE_COLUMNS <- "summary.json"
OUTPUT_FILE <- "joint_counts_comparison.tsv"

############################
# Global Vars
############################
SCRIPT_NAME <- "gs_vs_dig_jc_comparison.R"
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = file.path('analysis/jointCounts_analysis', SCRIPT_NAME))



flatten_joint_summary <- function(data){
    purrr::map2_dfr(data$recordId, data$filePath, 
                    function(curr_record, filePath){
                        tryCatch({
                            identifier_data <- fromJSON(filePath) %>% 
                                .$selectedIdentifier
                            if(nrow(identifier_data) == 0){
                                stop()
                            }
                            identifier_data %>% 
                                tibble::as_tibble() %>%
                                dplyr::mutate(recordId = curr_record)
                        }, error = function(e){
                            tibble(recordId = curr_record,
                                   isSelected = FALSE,
                                   error = geterrmessage())
                        }
                        )
                    }) %>% 
        dplyr::inner_join(data, by = c("recordId")) %>%
        dplyr::select(participantId, everything())
}


get_joint_summary <- function(){
    entity_gs_jc <- synTableQuery(glue::glue(
        "SELECT * FROM {GS_JOINT_COUNT}"))
    entity_dig_jc <- synTableQuery(glue::glue(
        "SELECT * FROM {DIG_JOINT_COUNT}"))
    tbl <- rbind(entity_gs_jc$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "gs_joint_count"), 
                 entity_dig_jc$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "dig_joint_count")) %>%
        dplyr::select(
            everything(), 
            fileHandleId = !!sym(FILE_COLUMNS))
    file_data <- list(
        gs_jc = synDownloadTableColumns(
            entity_gs_jc, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value),
        dig_jc = synDownloadTableColumns(
            entity_dig_jc, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value)) %>% 
        purrr::reduce(rbind)
    
    tbl %>% 
        dplyr::inner_join(
            file_data, by = c("fileHandleId"))
}

main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
    ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread()
    result <- get_joint_summary() %>%
        flatten_joint_summary() %>%
        join_with_ppacman(visit_ref_tbl = visit_ref, 
                          ppacman_tbl = ppacman) %>% 
        dplyr::group_by(participantId, 
                        createdOn, 
                        visit_num, 
                        fileColumnName, 
                        identifier) %>%
        dplyr::summarise(total = sum(isSelected)) %>% 
        ungroup() %>%
        drop_na(fileColumnName, identifier) %>%
        pivot_wider(
            names_from = fileColumnName, 
            values_from = total) %>%
        dplyr::mutate(dig_joint_count = 
                          ifelse(is.na(dig_joint_count), 0,  
                                 dig_joint_count)) %>%
        dplyr::mutate(gs_joint_count = 
                          ifelse(is.na(gs_joint_count), 0,  
                                 gs_joint_count)) %>%
        dplyr::mutate(
            jc_concordance = case_when(
                dig_joint_count == gs_joint_count ~ 0,
                dig_joint_count > gs_joint_count ~ -1, 
                TRUE ~ 1)) %>%
        dplyr::arrange(desc(createdOn)) %>%
        dplyr::mutate(createdOn = as.character(createdOn)) %>% 
        write_tsv(OUTPUT_FILE)
    file <- synapser::File(OUTPUT_FILE, PARENT_SYN_ID)
    synStore(file,
             activityName = "get jcounts comparison between gs and self-reported",
             used = c(PPACMAN_TBL_ID, 
                      VISIT_REF_ID,
                      DIG_JOINT_COUNT, 
                      GS_JOINT_COUNT), 
             executed = GIT_URL)
    unlink(OUTPUT_FILE)
}

main()
