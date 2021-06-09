########################################################################
# Psoriasis Validation
# Purpose: To extract features for the Digital Jar Open test
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

# source table ids
DIG_JAR_OPEN_TBL_ID <- 'syn22281747'
PARENT_SYN_ID <- 'syn22337134'
OUTPUT_FILENAME <- 'DigitalJarOpen_features.tsv'

# Github link
SCRIPT_PATH <- file.path('feature_extraction', "digitalJarOpener_features.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = SCRIPT_PATH)

main <- function(){
    digitalJarOpen.syn <- synapser::synTableQuery(paste0(
        'select * from ', DIG_JAR_OPEN_TBL_ID))
    digitalJarOpen.tbl <- digitalJarOpen.syn$asDataFrame() %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(djo_inward_ratio = 
                          max(leftClockwiseRotation, 
                              rightCounterRotation)/min(leftClockwiseRotation, 
                                                        rightCounterRotation),
                      djo_outward_ratio = 
                          max(rightClockwiseRotation, 
                              leftCounterRotation)/min(rightClockwiseRotation, 
                                                       leftCounterRotation)) %>% 
        dplyr::ungroup()
    digitalJarOpen.ftrs <- digitalJarOpen.tbl %>% 
        dplyr::select(-ROW_ID, -ROW_VERSION) %>% 
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        dplyr::select(recordId,
                      participantId,
                      createdOn,
                      djo_inward_ratio,
                      djo_outward_ratio,
                      djo_leftClockwise = leftClockwiseRotation,
                      djo_leftCounter = leftCounterRotation,
                      djo_rightClockwise = rightClockwiseRotation,
                      djo_rightCounter = rightCounterRotation) %>% 
        unique()
    
    save_to_synapse(data = digitalJarOpen.ftrs,
                    output_filename = OUTPUT_FILENAME,
                    parent = PARENT_SYN_ID,
                    name = "get dig jar opener feat",
                    executed = GIT_URL,
                    used = DIG_JAR_OPEN_TBL_ID)
    
}

log_process(main(), SCRIPT_PATH)
