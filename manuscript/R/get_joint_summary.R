############################################################
# This script will be used for summarizing reported each 
# joint counts for PSA users
# @author: aryton.tediarjo@sagebase.org
############################################################

# import libraries
library(synapser)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(jsonlite)
synLogin()
source("utils/feature_extraction_utils.R")


############################
# Global Vars
############################
PARENT_SYN_ID <- "syn26840745"
GS_JOINT_COUNT <- "syn22281781"
GS_JOINT_SWELL <- "syn22281780"
PPACMAN_TBL_ID <- "syn22337133"
VISIT_REF_ID <- "syn25825626"
FILE_COLUMNS <- "summary.json"
OUTPUT_FILE <- "joint_avg_reported.tsv"

############################
# Global Vars
############################
SCRIPT_NAME <- "get_joint_summary.R"
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = file.path('analysis/jointCounts_analysis', SCRIPT_NAME))


get_gs_joint_summaries <- function(){
    entity_list <- list()
    entity_list$gs_jc = synTableQuery(glue::glue(
            "SELECT * FROM {GS_JOINT_COUNT}"))
    entity_list$gs_js = synTableQuery(glue::glue(
            "SELECT * FROM {GS_JOINT_SWELL}"))
    tbl <- purrr::map(names(entity_list), function(column){
        entity_list[[column]]$asDataFrame() %>%
            dplyr::mutate(fileColumnName = column)}) %>% 
        dplyr::bind_rows() %>%
        dplyr::select(everything(), fileHandleId = !!sym(FILE_COLUMNS))
    files <- entity_list %>% 
        purrr::map_dfr(function(entity){
            synDownloadTableColumns(entity, FILE_COLUMNS) %>% 
                tibble::enframe() %>%
                dplyr::select(fileHandleId = name, filePath = value)})
    tbl %>% 
        dplyr::inner_join(
            files, by = c("fileHandleId")) %>% 
        tibble::as_tibble()
}

summarize_joint_by_average <- function(data){
    detected_psa_users <- (data$participantId %>% unique() %>% length())
    summary <- data %>% 
        dplyr::group_by(identifier) %>% 
        dplyr::summarise(
            sum_jc = n_distinct(participantId[(fileColumnName == "gs_jc") &
                                                  (isSelected == TRUE)]),
            sum_js = n_distinct(participantId[(fileColumnName == "gs_js") &
                                                  (isSelected == TRUE)])) %>%
        dplyr::mutate(sum_reported = sum_jc + sum_js,
                      avg_reported = sum_reported/detected_psa_users) %>%
        dplyr::arrange(desc(avg_reported))
}

main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
    ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread()
    result <- get_gs_joint_summaries() %>%
        flatten_joint_summary() %>%
        join_with_ppacman(visit_ref, ppacman) %>%
        dplyr::filter(diagnosis == "PsA") %>%
        summarize_joint_by_average() %>% 
        readr::write_tsv(OUTPUT_FILE)
    file <- synapser::File(OUTPUT_FILE, PARENT_SYN_ID)
    synStore(file, 
             activityName = "get each joint location summary",
             used = c(PPACMAN_TBL_ID,
                      VISIT_REF_ID,
                      GS_JOINT_SWELL, 
                      GS_JOINT_COUNT), 
             executed = GIT_URL)
    unlink(OUTPUT_FILE)
}

main()

