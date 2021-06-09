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


############################
# Global Vars
############################
PARENT_SYN_ID <- "syn24180371"
MD_JOINT_COUNT <- "syn22281781"
VISIT_REF <- "syn25825626"
USER_JOINT_COUNT <- "syn22281786"
PPACMAN_TBL <- "syn25006883"
FILE_COLUMNS <- "summary.json"
OUTPUT_FILE <- "joint_counts_comparison.tsv"
SCRIPT_NAME <- "get_jcounts_md_vs_self_reported_comparison.R"
GIT_PATH <- "~/git_token.txt"
GIT_REPO <- "arytontediarjo/psorcastValidationAnalysis"


############################
# Global Vars
############################
githubr::setGithubToken(readLines(GIT_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = file.path('analysis', SCRIPT_NAME))



join_with_ppacman <- function(data, visit_ref_tbl_id, ppacman_tbl_id){
    #' get visit reference and ppacman tbl
    visit_ref <- synGet(visit_ref_tbl_id)$path %>% fread()
    ppacman_tbl <- synapser::synTableQuery(
        glue::glue("SELECT * FROM {ppacman_tbl_id}"))$asDataFrame() %>% 
        dplyr::select(
            participantId,
            createdOn = `Date`,
            diagnosis = Diagnosis,
            visit_num = `Visit Number`)
    
    #' only visiting once
    single_visit_user <- visit_ref %>% 
        dplyr::filter(!has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' visit multiple times
    multiple_visit_user <- visit_ref %>% 
        dplyr::filter(has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' join multiple visits and single-visits based on precondition
    visit_data <- list(
        single_visit = data %>% 
            dplyr::filter(participantId %in% single_visit_user) %>%
            dplyr::inner_join(ppacman_tbl, by = c("participantId")) %>%
            dplyr::mutate(createdOn = createdOn.y),
        multiple_visit = data %>% 
            dplyr::filter(participantId %in% multiple_visit_user) %>%
            dplyr::full_join(visit_ref, by = c("participantId")) %>%
            dplyr::filter(createdOn > min_createdOn & createdOn < max_createdOn)) %>% 
        bind_rows()
    return(visit_data)
}


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
        dplyr::inner_join(data, by = c("recordId"))
}


get_gs_joint_summaries <- function(){
    entity_md_jc <- synTableQuery(glue::glue(
        "SELECT * FROM {MD_JOINT_COUNT}"))
    entity_user_jc <- synTableQuery(glue::glue(
        "SELECT * FROM {USER_JOINT_COUNT}"))
    tbl <- rbind(entity_md_jc$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "md_joint_count"), 
                 entity_user_jc$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "self_reported_joint_count")) %>%
        dplyr::select(
            everything(), 
            fileHandleId = !!sym(FILE_COLUMNS))
    file_data <- list(
        md_jc = synDownloadTableColumns(
            entity_md_jc, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value),
        user_jc = synDownloadTableColumns(
            entity_user_jc, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value)) %>% 
        purrr::reduce(rbind)
    
    tbl %>% 
        dplyr::inner_join(
            file_data, by = c("fileHandleId"))
}

main <- function(){
    result <- get_gs_joint_summaries() %>%
        flatten_joint_summary() %>%
        dplyr::select(participantId, everything()) %>%
        join_with_ppacman(visit_ref_tbl_id = VISIT_REF, 
                          ppacman_tbl_id = PPACMAN_TBL) %>% 
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
        dplyr::mutate(self_reported_joint_count = 
                          ifelse(is.na(self_reported_joint_count), 0,  
                                 self_reported_joint_count)) %>%
        dplyr::mutate(md_joint_count = 
                          ifelse(is.na(md_joint_count), 0,  
                                 md_joint_count)) %>%
        dplyr::mutate(
            jc_concordance = case_when(
                self_reported_joint_count == md_joint_count ~ 0,
                self_reported_joint_count > md_joint_count ~ -1, 
                TRUE ~ 1)) %>%
        dplyr::arrange(desc(createdOn)) %>%
        dplyr::mutate(createdOn = as.character(createdOn)) %>% 
        write_tsv(OUTPUT_FILE)
    file <- synapser::File(OUTPUT_FILE, PARENT_SYN_ID)
    synStore(file,
             activityName = "get jcounts comparison between md and self-reported",
             used = c(PPACMAN_TBL, 
                      VISIT_REF,
                      USER_JOINT_COUNT, 
                      MD_JOINT_COUNT), 
             executed = GIT_URL)
    unlink(OUTPUT_FILE)
}

main()
