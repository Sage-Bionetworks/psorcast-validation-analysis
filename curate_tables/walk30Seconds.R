############################################
#' Script to curate walk 30 seconds from 
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
SCRIPT_PATH <- "curate_tables/walk30Seconds.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
  GIT_REPO, repositoryPath = SCRIPT_PATH,
  ref = "branch",
  refName = 'main')

##############
# global parameters
##############
PARENT_SYN_ID <- 'syn22276946'
WALK_TBL_ID_V1 <- 'syn18387379'
WALK_TBL_ID_V2 <- 'syn20546394'
OUTPUT_TBL_NAME <- 'Walk30Seconds-Curated'
KEEP_COLS <- c("recordId",
               "appVersion",
               "externalId",
               "dataGroups",
               "createdOn",
               "createdOnTimeZone")
FILE_HANDLE_COLS <- "walk_motion.json"
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
      "where participantID like 'sub%' ",
      "OR participantID like 'SIT%' ",
      "OR participantID like 'Sub%' ",
      "OR participantID like 'Sit%'"   
  ))$asDataFrame()
}

main <- function(){
  #' Steps:
  #' - query synapse ids
  #' - bind into a dataframe
  #' - copy filehandles
  #' - replace participantID to participantId
  #' - regenerate table from source
  walk_table <- 
    c(WALK_TBL_ID_V1, WALK_TBL_ID_V2) %>% 
    purrr::map_dfr(function(syn_id){
      query_table(syn_id) %>%
        copy_file_handles(
          tbl_id = syn_id,
          file_handle_cols = FILE_HANDLE_COLS)}) %>%
    dplyr::bind_rows() %>%
    dplyr::select(participantId = participantID, everything()) %>%
    regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                     parent = PARENT_SYN_ID, 
                     keep_cols = KEEP_COLS,
                     new_cols = NEW_COLS,
                     file_handle_cols = FILE_HANDLE_COLS,
                     src_tbl_id = c(WALK_TBL_ID_V2),
                     used = c(WALK_TBL_ID_V1, WALK_TBL_ID_V2),
                     executed = GIT_URL,
                     name = "curate walk 30s from Bridge")
}

log_process(main(), SCRIPT_PATH)
