############################################
#' Script to curate Dig. Jar Opener data from 
#' Psorcast Validation Project (Bridge) to 
#' Psorcast Validation Analysis 
#' 
#' Author: Aryton Tediarjo, Meghasyam Tummalacherla
#' Maintainer: aryton.tediarjo@sagebase.org
#############################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(githubr)
library(data.table)
library(dplyr)
library(tidyr)
library(synapser)
library(synapserutils)
library(config)
source('utils/curate_table_utils.R')
source('utils/processing_log_utils.R')

synapser::synLogin()

####################################
#' Git Reference
####################################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- "curate_tables/digitalJarOpen.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    GIT_REPO, repositoryPath = SCRIPT_PATH,
    ref = "branch",
    refName = 'main')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
DIG_JAR_OPEN <- 'syn21313962'
OUTPUT_TBL_NAME <- 'DigitalJarOpen-Curated'
FILE_HANDLE_COLS <-  c("leftClockwise_motion.json",
                       "leftCounter_motion.json",
                       "rightClockwise_motion.json",
                       "rightCounter_motion.json")
KEEP_COLS <- c("participantId", 
               "externalId", 
               "appVersion",
               "recordId", 
               "dataGroups", 
               "createdOn",
               "createdOnTimeZone",
               "leftClockwise_rotation", 
               "leftCounter_rotation",
               "leftClockwiseRotation", 
               "leftCounterRotation", 
               "rightClockwise_rotation", 
               "rightCounter_rotation",
               "rightClockwiseRotation", 
               "rightCounterRotation")
NEW_COLS <- list(
    Column(name = "participantId", 
           columnType = "STRING", 
           maximumSize = 40))

##############
# query clause
##############
query_table <- function(tbl){
    # remove participant id
    metadata <- KEEP_COLS[KEEP_COLS != "participantId"] %>% 
        paste(collapse = ", ")
    file_handle_cols <- glue::glue("'{FILE_HANDLE_COLS}'") %>% 
        paste(collapse = ",")
    synapser::synTableQuery(glue::glue(
        "select participantID, {metadata}, {file_handle_cols} ",
        "from {tbl} ",
        "where `participantID` like 'sub%' ",
        "OR `participantID` like 'SIT%' ",
        "OR `participantID` like 'Sub%' ",
        "OR `participantID` like 'Sit%'"   
    ))$asDataFrame()
}

calculate_rotation <- function(data){
    data %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(rightClockwiseRotation = sum(rightClockwise_rotation, rightClockwiseRotation, na.rm = T),
                      leftClockwiseRotation = sum(leftClockwise_rotation, leftClockwiseRotation, na.rm = T),
                      rightCounterRotation = sum(rightCounter_rotation, rightCounterRotation, na.rm = T),
                      leftCounterRotation = sum(leftCounter_rotation, leftCounterRotation, na.rm = T)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-rightClockwise_rotation, -rightCounter_rotation,
                      -leftClockwise_rotation, -leftCounter_rotation)
}

main <- function(){
    #' Steps:
    #' - query synapse ids
    #' - row bind to dataframe
    #' - Calculate sum of rotation for each row
    #' - replace participantID to participantId
    #' - Remove columns with _rotation
    #' - copy filehandles
    #' - regenerate table from source
    cols_to_keep <- KEEP_COLS[!(stringr::str_detect(KEEP_COLS, "_rotation"))]
    create_table <- c(DIG_JAR_OPEN) %>%
        purrr::map(function(syn_id)
            query_table(syn_id) %>%
                copy_file_handles(
                    tbl_id = syn_id,
                    file_handle_cols = FILE_HANDLE_COLS)) %>% 
        dplyr::bind_rows() %>%
        dplyr::select(participantId = participantID, 
                      everything())  %>%
        calculate_rotation() %>%
        regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                         parent = PARENT_SYN_ID,
                         keep_cols = cols_to_keep,
                         file_handle_cols = FILE_HANDLE_COLS,
                         new_cols = NEW_COLS,
                         src_tbl_id = c(DIG_JAR_OPEN),
                         used = c(DIG_JAR_OPEN),
                         executed = GIT_URL,
                         name = "curate digital jar opener")
}

log_process(main(), SCRIPT_PATH)
