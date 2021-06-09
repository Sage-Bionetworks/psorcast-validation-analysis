############################################
#' Script to curate Dig. Joint Counting data from 
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
library(tidylog)
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
SCRIPT_PATH <- "curate_tables/jointCounting.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    GIT_REPO, repositoryPath = SCRIPT_PATH,
    ref = "branch",
    refName = 'master')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
JOINT_COUNTING_V3 <- 'syn20697059'
JOINT_COUNTING_V4 <- 'syn21322451'
OUTPUT_TBL_NAME <- 'JointCounting-Curated'
FILE_HANDLE_COLS <- c("summary.json")
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
query_table <- function(ppacman_tbl){
    # remove participant id
    metadata <- KEEP_COLS[KEEP_COLS != "participantId"] %>% 
        paste(collapse = ", ")
    file_handle_cols <- glue::glue("'{FILE_HANDLE_COLS}'") %>% 
        paste(collapse = ",")
    synapser::synTableQuery(glue::glue(
        "select participantID, {metadata}, {file_handle_cols} ",
        "from {ppacman_tbl} ",
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
    create_table <- c(JOINT_COUNTING_V3, JOINT_COUNTING_V4) %>%
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
                         src_tbl_id = c(JOINT_COUNTING_V4),
                         used = c(JOINT_COUNTING_V3,
                                  JOINT_COUNTING_V4),
                         executed = GIT_URL,
                         name = "curate user jcounts")
}

log_process(main(), SCRIPT_PATH)






