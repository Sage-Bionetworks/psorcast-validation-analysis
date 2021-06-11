############################################
#' Script to curate Foot Imaging data from 
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
SCRIPT_PATH <- "curate_tables/footImaging.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    GIT_REPO, repositoryPath = SCRIPT_PATH,
    ref = "branch",
    refName = 'main')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
FOOT_IMAGING_TBL <- 'syn20609184'
OUTPUT_TBL_NAME <- 'FootImaging-Curated'
FILE_HANDLE_COLS <-  c("leftFoot.png", "leftFoot.jpg",
                       "rightFoot.png", "rightFoot.jpg",
                       "summaryImage.jpg")
KEEP_COLS <- c("participantId", 
               "externalId", 
               "appVersion",
               "recordId", 
               "dataGroups", 
               "createdOn",
               "createdOnTimeZone")
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


main <- function(){
    #' Steps:
    #' - query synapse ids
    #' - row bind to dataframe
    #' - replace participantID to participantId
    #' - copy filehandles
    #' - regenerate table from source
    create_table <- c(FOOT_IMAGING_TBL) %>%
        purrr::map(function(syn_id)
            query_table(syn_id) %>%
                copy_file_handles(
                    tbl_id = syn_id,
                    file_handle_cols = FILE_HANDLE_COLS)) %>% 
        dplyr::bind_rows() %>%
        dplyr::select(participantId = participantID, 
                      everything())  %>%
        regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                         parent = PARENT_SYN_ID,
                         keep_cols = KEEP_COLS,
                         file_handle_cols = FILE_HANDLE_COLS,
                         new_cols = NEW_COLS,
                         src_tbl_id = c(FOOT_IMAGING_TBL),
                         used = c(FOOT_IMAGING_TBL),
                         executed = GIT_URL,
                         name = "curate foot imaging")
}

log_process(main(), SCRIPT_PATH)






