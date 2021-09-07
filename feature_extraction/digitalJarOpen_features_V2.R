########################################################################
#' Script to extract features for DjO for both 
#' pre-calculated total rotation/ratios columns from Synapse Tables
#' and from .json files in the column
#' 
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
library(jsonlite)
library(githubr)
source("utils/feature_extraction_utils.R")
synapser::synLogin()

# source table ids
DJO_FTRS <- 'syn22281747'
PPACMAN_FTRS <- "syn22337133"
VISIT_REF <- "syn25825626"

FILE_COLUMNS <- c("leftClockwise_motion.json", 
                  "leftCounter_motion.json",
                  "rightClockwise_motion.json", 
                  "rightCounter_motion.json")
OUTPUT_REF <- list(
    feature = list(
        output = 'DigitalJarOpen_features_v2.tsv',
        parent = "syn22337134"),
    log = list(
        output = 'removed_rows_DigitalJarOpen_features_v2.tsv',
        parent = "syn25832341")
)

# Github link
SCRIPT_PATH <- file.path('feature_extraction', "digitalJarOpen_features_V2.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='model'), 
    repositoryPath = SCRIPT_PATH)



# TEST functions for DJO

# * We are counting only from the time they actually started
#   rotating the phone (min of 5), and when they end (max-5). We are cutting off
#   times they are idle at the start and end (mid activity idle times are counted)

# look for recordId "d2xpigr86T5qMEQ3EoG1pMx5", the following method needs more clean up to get
# the exact part where they are performing the rotation (idle parts in middle getting counted)
preprocessGyroDat <- function(filepath){
    dat <- jsonlite::fromJSON(filepath %>% as.character()) %>% 
        dplyr::filter(sensorType == 'gyro')
    dat$timestamp <- dat$timestamp - min(dat$timestamp, na.rm = T)
    
    datZ <- dat %>% 
        dplyr::select(timestamp,z) %>% 
        dplyr::mutate(zTheta = abs(diffinv(z)[1:length(z+1)])) %>%
        dplyr::mutate(testZ = c(diff(zTheta),0)) %>% 
        dplyr::filter(testZ > 0) 
    
    # min degree to get started - 2
    # max degree to stop is 2 less than max rotated
    
    minDegree = 5
    maxDegree = max(datZ$zTheta)-5
    
    datZ <- datZ %>% 
        dplyr::mutate(tCons = c(0,diff(timestamp))) %>% 
        dplyr::filter(tCons < 0.015) %>%  # 100Hz, ie 0.01 should ideally be diff b/w consecutive timestamps
        dplyr::filter(zTheta > minDegree,
                      zTheta < maxDegree)
    
    return(datZ)
}

## Feature 1:
# Total time to do the test*
getTimeFromDat <- function(filepath){
    datZ <- preprocessGyroDat(filepath)
    range_ts <- max(datZ$timestamp)-min(datZ$timestamp)
    data <- tibble::tibble(total_time = range_ts)
    return(data)
}


## Feature 2:
# Get ecdf values(percentile) for abs(rotation speed) for speeds that are (-4,-3.5,....0,0.5,1...,4)
# NOTE: percentile values will be inverted(i.e, p, 1-p) for clockwise and anti-clockwise activities (as direction of rotation is opposite)
#       so to overcome this we will be using magnitude of speed. look at testZ in preprocessGyroDat function
# Also functions as proxy to min and max speeds (low and high quantile respectively)
getPercentileValues <- function(filepath){
    datZ <- preprocessGyroDat(filepath)  
    aa <- ecdf(datZ$testZ)
    ecdfVals <- aa(seq(0,4,0.25)) %>% 
        `names<-`(paste0('omega_', seq(0,4,0.25))) %>%
        tibble::as_tibble_row()
    return(ecdfVals)
    
}

## Feature 3:
# really related to feature 2, i.e what speed is 0.01 quantile, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99
# says how fast and how long those fast tasks were perfomed/lasted. Someone performing a very fast
# test, you can expect the later tail (0.75, 0.9 )etc., to be close to each other
# someone performing a lower test, will have a lot of slow speeds, so expect 0.1, 0.25, 0.5, etc.,
# i.e the early tail to be closer together
getQuantileValues <- function(filepath){
    datZ <- preprocessGyroDat(filepath)  
    aa <- quantile(datZ$testZ, c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99))
    QuantileVals <- aa %>% 
        `names<-`(paste0('quantile_',c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99))) %>%
        tibble::as_tibble_row()
    return(QuantileVals)
}

#' Function to retrieve rotation data
#' and total rotation features
#' 
#' @param data 
#' @return featurized total rotation data
get_total_rotation <- function(data){
    data %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(total_rotation = sum(
            djo_leftCounter, djo_rightCounter,
            djo_leftClockwise, djo_rightClockwise))
}


#' Function to retrieve rotation ratios
#' 
#' @param data 
#' @return featurized rotation ratios data
get_rotation_ratios <- function(data){
    data %>% 
        dplyr::rowwise() %>% 
            dplyr::mutate(djo_inward_ratio = 
                              max(djo_leftClockwise, djo_rightCounter)/min(djo_leftClockwise, djo_rightCounter),
                          djo_outward_ratio = 
                              max(djo_rightClockwise, djo_leftCounter)/min(djo_rightClockwise, djo_leftCounter)) %>% 
            dplyr::ungroup()
}

#' Function to normalize activity colums (row-binded)
#' into column wise table that informs {activity_type}_{feature} 
#' for each participant id and their visit number
#' 
#' @param data 
#' @return data with normalized activit type
normalize_activity_to_cols <- function(data){
   data %>% 
        dplyr::select(-filePath) %>%
        dplyr::mutate(activityType = stringr::str_remove(fileColumnName, ".json")) %>% 
        tidyr::pivot_wider(id_cols = c("participantId", "createdOn"), 
                           names_from = "activityType", 
                           names_glue = "{activityType}_{.value}",
                           values_from = matches("omega|quantile|total")) 
}

#' Function to run sensor feature extraction
#' by separately running filepath into the
#' set of functions and bind it as a row 
#' 
#' @param data 
#' @return featurized sensor data
get_sensor_features <- function(filepath){
    list(
        timerange = filepath %>% getTimeFromDat(),
        percentile = filepath %>% getPercentileValues(),
        quantile = filepath %>% getQuantileValues()) %>% 
    purrr::reduce(dplyr::bind_cols)
}

main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- synGet(VISIT_REF)$path %>% 
        fread(quote = "")
    ppacman <- synGet(PPACMAN_FTRS)$path %>% 
        fread(quote = "") %>%
        dplyr::select(participantId, 
                      createdOn,
                      visit_num)
    
    #' get table and rename
    #' based on schema requirements
    djo_table <- get_table(
        DJO_FTRS, 
        file_columns = FILE_COLUMNS) %>%
        dplyr::select(
            everything(),
            djo_leftClockwise = leftClockwiseRotation,
            djo_leftCounter = leftCounterRotation,
            djo_rightClockwise = rightClockwiseRotation,
            djo_rightCounter = rightCounterRotation)
    
    #' get sets of djo features,
    #' map into sensor features and 
    #' total rotation features
    #' and inner join based on participantId and createdOn
    #' using reduce opreation -> join with ppacman to get 
    #' visit ref
    djo_features <- list(
        sensor_features = djo_table %>%
            dplyr::select(participantId, createdOn, fileColumnName, filePath) %>%
            dplyr::mutate(features = purrr::map(
                filePath, get_sensor_features)) %>%
            tidyr::unnest(features) %>%
            normalize_activity_to_cols(),
        rotation_features = djo_table %>% 
            dplyr::group_by(participantId, createdOn) %>%
            dplyr::summarise_all(last) %>%
            get_rotation_ratios() %>%
            get_total_rotation()) %>% 
        purrr::reduce(dplyr::full_join, by = c("participantId", "createdOn")) %>%
        join_with_ppacman(visit_ref, ppacman) %>%
        select(participantId, visit_num, everything())
        
    #' get removed features if any
    removed_rows <- djo_table %>% 
        distinct(recordId) %>% 
        dplyr::anti_join(djo_features, 
                         by = c("recordId"))
    
    #' save to synapse
    save_to_synapse(data = djo_features,
                    output_filename = OUTPUT_REF$feature$output,
                    parent = OUTPUT_REF$feature$parent,
                    name = "get djo features (sensor & total)",
                    executed = GIT_URL,
                    used = DJO_FTRS)
    save_to_synapse(data = removed_rows,
                    output_filename = OUTPUT_REF$log$output,
                    parent = OUTPUT_REF$log$parent,
                    name = "get djo feat - log",
                    executed = GIT_URL,
                    used = DJO_FTRS)
}

main()