########################################################################
#' Psoriasis Validation
#' 
#' Purpose: 
#' This script is used to extract
#' walking features (PDKit + Rotation) fron 30s walk data
#' Note: not using synapser due to conflicts
#' 
#' Author: Aryton Tediarjo
#' email: aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

library(reticulate)
library(tidyverse)
library(plyr)
library(jsonlite)
library(doMC)
library(githubr)
library(data.table)
library(jsonlite)
registerDoMC(detectCores())
source("utils/reticulate_python_utils.R")
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')

####################################
#### Global Variables ##############
####################################
GIT_PATH <- "~/git_token.txt"
WALK_TBL <- "syn22281384"
PPACMAN_TBL_ID <- "syn22337133"
VISIT_REF_ID <- "syn25825626"
UID <- c("recordId")
KEEP_METADATA <- c("participantId",
                   "createdOn", 
                   "appVersion")
REMOVE_FEATURES <- c("y_speed_of_gait", 
                     "x_speed_of_gait", 
                     "z_speed_of_gait", 
                     "AA_stride_regularity",
                     "AA_step_regularity", 
                     "AA_symmetry")
FILE_HANDLE_COLS <- c("walk_motion.json")
GROUP_COLS <- c("participantId","recordId", "createdOn") #' used for aggregating features

####################################
#### instantiate python objects #### 
####################################
gait_feature_py_obj <- reticulate::import(
    "PDKitRotationFeatures")$gait_module$GaitFeatures(
        detect_rotation = TRUE, sensor_window_size = 750L)
synapseclient <- reticulate::import("synapseclient")
syn <- synapseclient$login()


##############################
# Outputs
##############################
PARENT_SYN_ID <- "syn22336714"
ERROR_LOG_SYN_ID <- "syn25832341"
OUTPUT_FEATURE_FILE <- list(
    raw_features = "walk30s_raw_features.tsv", 
    agg_features = "walk30s_agg_features.tsv",
    log_data = "error_log_walk30s_features.tsv"
)

##############################
# Github token
##############################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
SCRIPT_PATH <- file.path('feature_extraction', "walk30s_features.R")
GIT_URL <- githubr::getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)

####################################
## Helpers
####################################

#' Function to search for gyroscope and accelerometer 
#' sensors in a time-series 
#' 
#' @param ts timeseries dataframe of mpower_v2 format (x,y,z,sensortype,timestamp)
#' 
#' @return list of named dataframe for accel and gyro
search_gyro_accel <- function(ts){
    #' split to list
    ts_list <- list()
    ts_list$acceleration <- ts %>% 
        dplyr::filter(
            stringr::str_detect(tolower(sensorType), "^accel"))
    ts_list$rotation <- ts %>% 
        dplyr::filter(
            stringr::str_detect(tolower(sensorType), "^rotation|^gyro"))
    
    #' parse rotation rate only
    if(length(ts_list$rotation$sensorType %>% unique(.)) > 1){
        ts_list$rotation <- ts_list$rotation %>% 
            dplyr::filter(!stringr::str_detect(tolower(sensorType), "^gyro"))
    }
    
    ts_list <- ts_list %>%
        purrr::map(., ~(.x %>%
                            dplyr::mutate(t = timestamp - .$timestamp[1]) %>%
                            dplyr::select(t,x,y,z)))
    return(ts_list)
}

#' Entry-point function to parse each filepath of each recordIds
#' walk data and featurize each record using featurize walk data function
#' @params data: dataframe containing filepaths
#' @returns featurized walk data for each participant 
process_walk_samples <- function(data, parallel=FALSE){
    features <- plyr::ddply(
        .data = data,
        .variables = all_of(c(UID, KEEP_METADATA, "fileColumnName")),
        .parallel = parallel,
        .fun = function(row){
            tryCatch({ # capture common errors
                ts <- jsonlite::fromJSON(row$filePath)
                if(nrow(ts) == 0){
                    stop("ERROR: sensor timeseries is empty")
                }else{
                    ts_list <- ts %>% search_gyro_accel()
                    gait_feature_py_obj$run_pipeline(
                        ts_list$acceleration, ts_list$rotation)
                }
            }, error = function(err){ # capture all other error
                error_msg <- stringr::str_squish(
                    stringr::str_replace_all(geterrmessage(), "\n", ""))
                return(tibble::tibble(error = error_msg))})}) %>%
        dplyr::mutate_at(all_of(KEEP_METADATA), as.character) %>% 
        replace(., . =="NaN", NA)
    return(features)
}

#' function to summarize features
#' @param walk_data the walk data from segmentation
#' @param rotation_data the rotation data from segmentation
#' @return aggregated data
summarize_features <- function(walk_data, 
                               rotation_data,
                               group_cols){
    features <- walk_data %>% 
        dplyr::select(matches("^x|^y|^z|^AA")) %>% names()
    summarize_walk <- walk_data %>% 
        dplyr::group_by(across(all_of(group_cols))) %>%
        dplyr::summarise_at(features, list("md" = median, 
                                           "iqr" = IQR,
                                           "var" = var), na.rm = TRUE)
    
    summarize_rotation <- rotation_data %>% 
        dplyr::group_by(across(all_of(group_cols))) %>%
        dplyr::summarise_at(c("rotation_omega"), 
                            list("rotation_omega_md" = median, 
                                 "rotation_omega_iqr" = IQR,
                                 "rotation_omega_var" = var), na.rm = TRUE)
    summarize_walk %>% 
        dplyr::left_join(summarize_rotation, by = group_cols)
}

#' function to segment gait data
#' by rotation and non-rotation
#' @param data dataframe/tibble of walk data
segment_gait_data <- function(data){
    result <- list()
    data <- data %>% 
        dplyr::filter(is.na(error))
    result$walk_data <- data %>% 
        dplyr::filter(is.na(rotation_omega)) 
    result$rotation_data <- data %>% 
        dplyr::filter(!is.na(rotation_omega))
    return(result)
}




main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- syn$get(VISIT_REF_ID)$path %>% fread()
    ppacman <- syn$get(PPACMAN_TBL_ID)$path %>% fread()
    
    #' get walk data and featurize
    walk_features <- get_table_py_client(
        syn, synapse_tbl = WALK_TBL, 
        file_columns = FILE_HANDLE_COLS) %>% 
        process_walk_samples(parallel = T) %>%
        dplyr::select(-all_of(REMOVE_FEATURES))
    
    #' segment walk data
    segment_walk_data <- walk_features %>% 
        segment_gait_data()
    
    #' aggregate_features
    all_aggregate_features <- summarize_features(
        segment_walk_data$walk_data,
        segment_walk_data$rotation_data,
        GROUP_COLS)
    
    #' subset joinnable data with ppacman and duplicate entries
    aggregate_features <- all_aggregate_features %>%
        join_with_ppacman(visit_ref, ppacman) %>% 
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        dplyr::select(recordId, 
                      participantId, 
                      createdOn, 
                      visit_num, 
                      ends_with("iqr"),
                      ends_with("md")) %>% 
        dplyr::group_by(participantId, visit_num) %>%
        dplyr::summarise_all(last)
    
    #' removed_file 
    log_data <- log_removed_data(all_aggregate_features %>%
                                     dplyr::mutate(error = NA), aggregate_features)
    
    #' store raw features to synapse
    save_to_synapse_py_client(syn = syn, 
                    synapseclient = synapseclient, 
                    data = walk_features, 
                    output_filename = OUTPUT_FEATURE_FILE$raw_features, 
                    parent = PARENT_SYN_ID,
                    name = "get raw walk features",
                    used = WALK_TBL,
                    executed = GIT_URL)
    
    
    #' store aggregate features to synapse
    save_to_synapse_py_client(syn = syn, 
                    synapseclient = synapseclient, 
                    data = aggregate_features, 
                    output_filename = OUTPUT_FEATURE_FILE$agg_features, 
                    parent = PARENT_SYN_ID,
                    name = "get aggregate walk features",
                    used = WALK_TBL,
                    executed = GIT_URL)
    
    #' store log data from which recordIds are removed
    save_to_synapse_py_client(syn = syn, 
                    synapseclient = synapseclient, 
                    data =log_data, 
                    output_filename = OUTPUT_FEATURE_FILE$log_data, 
                    parent = ERROR_LOG_SYN_ID,
                    name = "get log from removed records",
                    used = WALK_TBL,
                    executed = GIT_URL)
}

log_process(main(), SCRIPT_PATH)
