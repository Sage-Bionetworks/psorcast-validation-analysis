############################################################
#' This script will be used for summarizing reported each 
#' joint counts for Psorcast users 
#' 
#' Note: it will exclude finger joints and only use
#' main joint location (ankle, hip, elbow, wrist etc.)
#' 
#' @author: aryton.tediarjo@sagebase.org
#' @maintainer: aryton.tediarjo@sagebase.org
############################################################
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
PARENT_SYN_ID <- "syn22336715"
ERROR_LOG_SYN_ID <- "syn25832341"
VISIT_REF <- "syn25825626"
PPACMAN_TBL <- "syn25006883"
FILE_COLUMNS <- "summary.json"
JOINT_LOCATION <- c("knee", "hip", "ankle", 
                    "wrist", "elbow", "shoulder")
OUTPUT_FILE <- "joint_counts_comparison.tsv"
JOINT_TBL_REF <- list(
    dig_jc = list(
        prefix = "dig_jc",
        syn_id = "syn22281786",
        output_filename = "dig_jc_features.tsv"),
    md_jc = list(
        prefix = "gs_jc",
        syn_id = "syn22281781",
        output_filename = "gs_jc_features.tsv"
    ),
    md_swell = list(
        prefix = "gs_swell",
        syn_id = "syn22281780",
        output_filename = "gs_swell_features.tsv"
    )
)


############################
# Git Reference
############################
SCRIPT_PATH <- file.path('feature_extraction', "jointSummaries_features.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = SCRIPT_PATH)


#' function to group each identifiers 
#' @data dataframe of flattened summary.json
calculate_reported_counts <- function(data){
    data %>%
        dplyr::filter(is.na(error)) %>%
        dplyr::group_by(recordId) %>%
        dplyr::summarise(counts = sum(isSelected, na.rm = T)) %>%
        dplyr::arrange(desc(counts))
}

#' function to get string colums of reported joint count
#' @data dataframe of flattened summary.json
create_joint_string_list <- function(data){
    data %>%
        dplyr::filter(is.na(error)) %>%
        dplyr::group_by(recordId) %>%
        dplyr::summarise(joint_list = paste0(identifier, collapse = ","))
}

#' function to parse pain status symmetry
#' @data dataframe of flattened summary.json
parse_joint_pain_symmetry <- function(data){
    detected_joints <- data %>% 
        drop_na(identifier_group) %>%
        .$identifier_group %>% 
        unique()
    purrr::map(detected_joints , function(joint_identifier){
        output_cols <- glue::glue("status_{joint_identifier}")
        left_side_cols <- glue::glue("left_{joint_identifier}")
        right_side_cols <- glue::glue("right_{joint_identifier}")
        data %>% 
            dplyr::filter(is.na(identifier) | 
                              str_detect(identifier, joint_identifier)) %>%
            pivot_wider(names_from = identifier, 
                        values_from = isSelected) %>% 
            dplyr::mutate(!!sym(output_cols) := 
                              case_when((!!sym(right_side_cols) == TRUE & 
                                             !!sym(left_side_cols) == TRUE) ~ "both",
                                        (!!sym(right_side_cols) == TRUE & 
                                             !!sym(left_side_cols) == FALSE) ~ "right",
                                        (!!sym(right_side_cols) == FALSE & 
                                             !!sym(left_side_cols) == TRUE) ~ "left",
                                        TRUE ~ NA_character_)) %>%
            dplyr::select(recordId, !!sym(output_cols))}) %>% 
        purrr::reduce(dplyr::full_join, by = c("recordId"))
}

#' function to get joint report based on synapseID
#' @syn_id: synapse id of the joint report tables
get_joint_report <- function(syn_id){
    get_table(syn_id, file_columns = FILE_COLUMNS) %>% 
        flatten_joint_summary() %>%
        dplyr::group_by(recordId, 
                        createdOn,
                        participantId,
                        identifier) %>%
        dplyr::summarise_all(last)  %>% 
        dplyr::mutate(
            identifier_group = 
                str_extract(identifier, 
                            str_c(JOINT_LOCATION, 
                                  collapse = "|")))
}


main <- function(){
    #' - Go through each synapse id
    #' - For each table flatten all the summary.json files
    #' - Calculate metrics:
    #' a. reported joint counts (group-by of record and identifier)
    #' b. parse into string for all major joints
    #' c. parse symmetrical pain status
    joint_summaries <- purrr::map(names(JOINT_TBL_REF), function(activity){
        #' retrieve data
        prefix <- JOINT_TBL_REF[[activity]]$prefix
        output_filename <- JOINT_TBL_REF[[activity]]$output_filename
        tbl_id <- JOINT_TBL_REF[[activity]]$syn_id
        joint_report <- get_joint_report(tbl_id) %>% dplyr::ungroup()
        
        #' get each metrics
        metrics <- 
            list(
                joint_count = calculate_reported_counts(joint_report),
                joint_str_list = create_joint_string_list(joint_report),
                joint_pain_status = parse_joint_pain_symmetry(joint_report)) %>% 
            purrr::reduce(dplyr::full_join, 
                          by = c("recordId")) %>%
            dplyr::mutate(counts = ifelse(is.na(counts), 0, counts)) %>%
            dplyr::rename_with(~paste0(prefix, "_", .), 
                               -c("recordId"))
        
        #' clean data 
        counts_columns <- paste0(prefix, "_", "counts")
        joint_data <- joint_report %>%
            distinct(recordId, participantId, createdOn) %>%
            dplyr::left_join(metrics, by = c("recordId")) %>%
            dplyr::arrange(desc(createdOn)) %>%
            dplyr::mutate(error = ifelse(is.na(!!sym(counts_columns)), 
                                         "error: empty list in summary.json",
                                         NA_character_)) %>%
            dplyr::filter(is.na(error))
        
        #' get error logging for removed records
        error_log <- joint_data %>% 
            dplyr::select(recordId, 
                          createdOn, 
                          participantId, 
                          error) %>%
            dplyr::filter(!is.na(error))
        
        #' save joint features to synapse
        save_to_synapse(data = joint_data %>% 
                            dplyr::select(-error),
                        output_filename = output_filename, 
                        parent = PARENT_SYN_ID,
                        name = "get joint summaries",
                        executed = GIT_URL,
                        used = tbl_id)
        
        #' save error log to synapse
        save_to_synapse(data = error_log,
                        output_filename = glue::glue("error_log_", output_filename), 
                        parent = ERROR_LOG_SYN_ID,
                        name = "get error log for joint summaries",
                        executed = GIT_URL,
                        used = tbl_id)
    })
}

log_process(main(), SCRIPT_PATH)


