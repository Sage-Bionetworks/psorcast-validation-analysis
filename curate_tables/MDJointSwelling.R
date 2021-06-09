############################################
#' Script to curate MD Joint Swelling data from 
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
GIT_TOKEN_PATH <- "~/git_token.txt"
GIT_REPO <- "arytontediarjo/psorcastValidationAnalysis"
SCRIPT_PATH <- "curate_tables/MDJointSwelling.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    GIT_REPO, repositoryPath = SCRIPT_PATH,
    ref = "branch",
    refName = 'master')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
MD_JOINT_SWELLING <- 'syn20744314'
OUTPUT_TBL_NAME <- 'MDJointSwelling-Curated'
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
    #' - replace participantID to participantId
    #' - copy filehandles
    #' - regenerate table from source
    create_table <- 
        query_table(MD_JOINT_SWELLING) %>%
        dplyr::select(participantId = participantID, 
                      everything()) %>%
        copy_file_handles(
            tbl_id = MD_JOINT_SWELLING,
            file_handle_cols = FILE_HANDLE_COLS) %>%
        regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                         parent = PARENT_SYN_ID,
                         keep_cols = KEEP_COLS,
                         file_handle_cols = FILE_HANDLE_COLS,
                         new_cols = NEW_COLS,
                         src_tbl_id = c(MD_JOINT_SWELLING),
                         used = c(MD_JOINT_SWELLING),
                         executed = GIT_URL,
                         name = "curate MD joint swelling from bridge")
}

log_process(main(), SCRIPT_PATH)
