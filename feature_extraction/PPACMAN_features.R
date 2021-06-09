########################################################################
# Psoriasis Validation
# Purpose: To extract features from the PPACMAN assessor table
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
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
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')


synapser::synLogin()

# output reference
PPACMAN_SYN_ID <- 'syn25006883'
PARENT_SYN_ID <- 'syn22337132' # synId of folder to upload your file to
OUTPUT_FILE <- 'PPACMAN_assessor_features.tsv' # name your file

# Github link
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
SCRIPT_PATH <- 'feature_extraction/PPACMAN_features.R'
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = SCRIPT_PATH)

main <- function(){
    ppacman.syn <- synapser::synTableQuery(paste0('select * from ', PPACMAN_SYN_ID))
    ppacman.tbl <- ppacman.syn$asDataFrame()
    
    ppacman.ftrs <- ppacman.tbl %>% 
        dplyr::select(-ROW_ID, -ROW_VERSION) %>% 
        dplyr::select(participantId,
                      createdOn = `Date`,
                      visit_num = `Visit Number`,
                      age = Age,
                      sex = Sex,
                      diagnosis = Diagnosis,
                      finger_dactylitis = `Finger Dactylitis`,
                      toe_dactylitis = `Toe Dactylitis`,
                      finger_nail = `Finger Nail Involvement`,
                      toe_nail = `Toe Nail Involvement`,
                      tjc_backup = `(Optional backup) MD TJC`,
                      sjc_backup = `(Optional backup) MD SJC`,
                      enthesitis = `Enthesitis Notes`,
                      gs_bsa = `Overall BSA (%)`) %>%
        dplyr::mutate(tjc_backup = stringr::str_replace_all(tjc_backup, "[\r\n]|:" , ""),
                      sjc_backup = stringr::str_replace_all(sjc_backup, "[\r\n]|:" , ""))
    
    #' write to synapse
    save_to_synapse(data = ppacman.ftrs,
                    output_filename = OUTPUT_FILE, 
                    parent = PARENT_SYN_ID, 
                    name = "get ppacman features",
                    executed = GIT_URL, 
                    used = PPACMAN_SYN_ID)
}

log_process(main(), SCRIPT_PATH)
