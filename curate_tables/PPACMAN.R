############################################
#' Script to curate PPACMAN data from 
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
SCRIPT_PATH <- "curate_tables/PPACMAN.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    GIT_REPO, repositoryPath = SCRIPT_PATH,
    ref = "branch",
    refName = 'main')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
PPACMAN_TBL <- 'syn21633790'
OUTPUT_TBL_NAME <- 'PPACMAN_assessor_data-Curated'
FILE_HANDLE_COLS <- "walk_motion.json"
NEW_COLS <- list(
    Column(name = "participantId", 
           columnType = "STRING", 
           maximumSize = 40))

##############
# query clause
##############
query_table <- function(ppacman_tbl){
    synapser::synTableQuery(glue::glue(
        "select * ",
        "from {ppacman_tbl} ",
        "where `Participant ID` like 'sub%' ",
        "OR `Participant ID` like 'SIT%' ",
        "OR `Participant ID` like 'Sub%' ",
        "OR `Participant ID` like 'Sit%'"   
    ))$asDataFrame()
}

main <- function(){
    #' Steps:
    #' - query synapse ids
    #' - replace participantID to participantId
    #' - regenerate table from source
    create_table <- 
        query_table(PPACMAN_TBL) %>%
        dplyr::select(participantId = `Participant ID`, 
                      everything()) %>%
        regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                         parent = PARENT_SYN_ID,
                         keep_cols = names(.),
                         new_cols = NEW_COLS,
                         src_tbl_id = c(PPACMAN_TBL),
                         used = c(PPACMAN_TBL),
                         executed = GIT_URL,
                         name = "curate ppacman table")
}

log_process(main(), SCRIPT_PATH)
