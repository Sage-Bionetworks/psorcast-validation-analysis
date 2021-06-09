############################################
#' Script to curate psoriasis draw from 
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
library(tidyr)
library(plyr)
library(dplyr)
library(synapser)
library(synapserutils)
library(config)
source('utils/curate_table_utils.R')
source('utils/processing_log_utils.R')

synapser::synLogin()

##############
# Global Parameters
##############
PARENT_SYN_ID <- 'syn22276946'
PSORIASIS_DRAW_TBL <- 'syn20835605'
OUTPUT_TBL_NAME <- 'PsoriasisDraw-Curated'
FILE_HANDLE_COLS <- c("summaryImage.png",
                      "selectedZones.json",
                      "summaryImage.jpg")
KEEP_COLS <- c(
  "participantId",
  "recordId",
  "appVersion",
  "externalId",
  "dataGroups",
  "createdOn",
  "createdOnTimeZone",
  "aboveTheWaistFrontCoverage", 
  "aboveTheWaistBackCoverage",
  "belowTheWaistFrontCoverage", 
  "belowTheWaistBackCoverage",
  "coverage"
)

NEW_COLS <- list(
  Column(name = "participantId", 
         columnType = "STRING", 
         maximumSize = 40))

####################################
#' Git Reference
####################################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- "curate_tables/psoriasisDraw.R"
setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
  GIT_REPO, repositoryPath = SCRIPT_PATH,
  ref = "branch",
  refName = 'master')


#############
# Helper
#############
query_table <- function(tbl){
  metadata <- KEEP_COLS[KEEP_COLS != "participantId"] %>% 
    paste(collapse = ", ")
  file_handle_cols <- glue::glue("'{FILE_HANDLE_COLS}'") %>% 
    paste(collapse = ",")
  
  # get query
  synapser::synTableQuery(glue::glue(
    "select participantID, {metadata}, {file_handle_cols} ", 
    "from {tbl} ", 
    "where participantID like 'sub%' ",
    "OR participantID like 'SIT%' ",
    "OR participantID like 'Sub%' ",
    "OR participantID like 'Sit%'"))$asDataFrame()
}


main <- function(){
  #' Steps:
  #' - query synapse ids
  #' - copy filehandles
  #' - replace participantID to participantId
  #' - regenerate table from source
  PSORIASIS_DRAW_TBL %>% 
    query_table() %>% 
    dplyr::select(participantId = participantID, everything()) %>%
    copy_file_handles(
      tbl_id = PSORIASIS_DRAW_TBL,
      file_handle_cols = FILE_HANDLE_COLS) %>%
    regenerate_table(tbl_name = OUTPUT_TBL_NAME,
                     parent = PARENT_SYN_ID, 
                     keep_cols = KEEP_COLS,
                     new_cols = NEW_COLS,
                     file_handle_cols = FILE_HANDLE_COLS,
                     src_tbl_id = c(PSORIASIS_DRAW_TBL),
                     used = c(PSORIASIS_DRAW_TBL),
                     executed = GIT_URL,
                     name = "curate psoriasis draw")
}

log_process(main(), SCRIPT_PATH)
