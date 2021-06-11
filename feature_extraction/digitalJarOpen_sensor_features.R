########################################################################
#' Psoriasis Validation
#' Purpose: To extract features for the Digital Jar Open test
#' using sensor data (motion.json) instead of pre-calculated
#' rotation from iOS
#' Maintainer: aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(githubr)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(furrr)
library(jsonlite)
library(githubr)
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')

future::plan(multicore)
synapser::synLogin()

# source table ids
DIG_JAR_OPEN_TBL_ID <- 'syn22281747'
PARENT_SYN_ID <- 'syn22337134'
OUTPUT_FILENAME <- 'DigitalJarOpen_mhealthtools_features.tsv'
FILE_COLUMNS <- c("leftClockwise_motion.json", 
                  "leftCounter_motion.json",
                  "rightClockwise_motion.json", 
                  "rightCounter_motion.json")

# Github link
SCRIPT_PATH <- file.path('feature_extraction', "digitalJarOpen_sensor_features.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)


#' get mhealthtools features using the given filepath
#' and search for accelerometer and gyroscope parameter
#' from sensor data
#' @param file_path: json filepath to sensor data
#' @return a named list of accel and gyroscope reading
#' with (t,x,y,z) column format
get_mhealthtools_features <- function(file_path) {
        tryCatch({
            data_list <- jsonlite::fromJSON(file_path) %>%
                search_gyro_accel()
            feature_list <- list(
                accel = mhealthtools::accelerometer_features(
                    data_list[["acceleration"]],
                    detrend = TRUE,
                    frequency_filter = c(4, 16),
                    window_length = 512,
                    window_overlap = 0.25,
                    derived_kinematics = TRUE,
                    funs = c(mhealthtools::time_domain_summary,
                             mhealthtools::frequency_domain_summary)),
                gyro = mhealthtools::gyroscope_features(
                    data_list[["rotation"]],
                    detrend = TRUE,
                    frequency_filter = c(4, 16),
                    window_length = 512,
                    window_overlap = 0.5,
                    derived_kinematics = TRUE,
                    funs = c(mhealthtools::time_domain_summary,
                             mhealthtools::frequency_domain_summary)))
            if(!is.null(feature_list$accel$error) | !is.null(feature_list$accel$error)){
                features <- tibble::tibble(error = 
                                               glue::glue(feature_list$accel$error, 
                                                          feature_list$accel$error))
            }else{
                features <- 
                    purrr::map_dfr(names(feature_list), 
                                   function(sensorType){
                        feature_list[[sensorType]]$extracted_features %>% 
                            dplyr::mutate(sensorType = sensorType) %>%
                            dplyr::mutate(error = NA_character_)})
            }
            return(features)
        }, errror = function(e){
            error_msg <- stringr::str_squish(
                stringr::str_replace_all(geterrmessage(), "\n", ""))
            return(tibble::tibble(error = error_msg))
        })
}


main <- function(){
    #' - get table from synapse and file handle columns
    #' - format data into tidy format (with recordId, and filehandlecolumns as index)
    #' - for each sensor filepaths in index, parallel process using mhealthtools
    #' - use mhealthtools to parse time-domain/frequency-domain features
    #' - clean columns based on desired metadata and desired features
    #' - save to synapse
    djo_mhealthtools_features <- 
        get_table(DIG_JAR_OPEN_TBL_ID, 
                  file_columns = FILE_COLUMNS) %>% 
        parallel_process_samples(funs = get_mhealthtools_features) %>%
        dplyr::select(recordId, 
                      createdOn, 
                      participantId, 
                      measurementType, 
                      sensorType,
                      starts_with("window"),
                      ends_with(".tm"),
                      ends_with(".fr")) %>%
        save_to_synapse(
            data = .,
            output_filename = OUTPUT_FILENAME,
            parent = PARENT_SYN_ID,
            name = "get dig jar opener mhealthtools feat",
            executed = GIT_URL,
            used = DIG_JAR_OPEN_TBL_ID)
}

log_process(main(), SCRIPT_PATH)

    
