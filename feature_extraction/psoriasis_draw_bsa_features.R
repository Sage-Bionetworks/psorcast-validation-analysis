########################################################################
#' Psoriasis Validation
#' 
#' Purpose: 
#' This script is used to extract
#' psoriasis draw digital body surface area
#' 
#' TODO:
#' Use selectedZones.json
#' 
#' Author: Aryton Tediarjo
#' email: aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

##############################
# Libraries
##############################
library(synapser)
library(jsonlite)
library(plyr)
library(tidyverse)
library(purrr)
library(githubr)
library(readr)
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')

synapser::synLogin()

##############################
# Global Variables
##############################
## source table ids
PSORIASIS_DRAW_TBL_ID <- "syn22281746"
PARENT_SYN_ID <- "syn22336716"
OUTPUT_FILENAME <- "psoriasisDraw_features.tsv"

## list of digital body surface multipliers (percentage %)
ABOVE_WAIST_FRONT_WEIGHT<- 25.6873160187267/100
ABOVE_WAIST_BACK_WEIGHT <- 22.7544780091461/100
BELOW_WAIST_FRONT_WEIGHT <- 25.7628094415659/100
BELOW_WAIST_BACK_WEIGHT <- 25.7953965305613/100

##############################
# Outputs
##############################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- file.path('feature_extraction', "psoriasis_draw_bsa_features.R")
setGithubToken(
    readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)

##############################
# Helper
##############################
#' function to recalculate coverage
#' based on different app version 
#' @data tibble/dataframe of psoDraw
calculate_coverage <- function(data){
    data %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(dig_bsa = sum(c(
            aboveTheWaistFrontCoverage * ABOVE_WAIST_FRONT_WEIGHT, 
            aboveTheWaistBackCoverage * ABOVE_WAIST_BACK_WEIGHT,
            belowTheWaistFrontCoverage * BELOW_WAIST_FRONT_WEIGHT, 
            belowTheWaistBackCoverage * BELOW_WAIST_BACK_WEIGHT), 
            na.rm = TRUE)) %>%
        tidyr::separate(appVersion, c("_1", "_2", "_3", "build"), sep = " ")  %>% 
        dplyr::select(-c("_1", "_2", "_3")) %>%
        dplyr::mutate(dig_bsa = case_when(
            build < 20 ~ dig_bsa * .5,
            TRUE ~ dig_bsa * 1))
}

main <- function(){
    #' retrieve table and recalculate coverage based on version
    pso_draw_table <- get_table(PSORIASIS_DRAW_TBL_ID) %>%
        calculate_coverage() %>%
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        dplyr::arrange(desc(createdOn)) %>%
        dplyr::select(-coverage) %>%
        dplyr::select(recordId, 
                      createdOn, 
                      participantId, 
                      ends_with("coverage"),
                      dig_bsa)
    
    #' save pso draw dable
    save_to_synapse(data = pso_draw_table,
                    output_filename = OUTPUT_FILENAME,
                    parent = PARENT_SYN_ID,
                    name = "get psodraw features",
                    executed = GIT_URL,
                    used = PSORIASIS_DRAW_TBL_ID)
}

log_process(main(), SCRIPT_PATH)

