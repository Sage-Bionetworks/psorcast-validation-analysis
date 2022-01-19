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
library(data.table)
source("manuscript/utils/feature_extraction_utils.R")
source('manuscript/utils/processing_log_utils.R')
source("manuscript/utils/fetch_id_utils.R")

synapser::synLogin()

##############################
# Global Variables
##############################
## source table ids
PSORIASIS_DRAW_TBL_ID <- "syn22281746"
PPACMAN_TBL_ID <- SYN_ID_REF$ppacman
VISIT_REF_ID <- SYN_ID_REF$visit_summary
OUTPUT_REF <- list(
    feature = list(
        output = "psoriasisDraw_features.tsv",
        parent = "syn26840744"),
    log = list(
        output = "error_log_psoriasisDraw_features.tsv",
        parent = "syn26844313")
    )
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
        dplyr::mutate(
            dig_bsa = case_when(
                build < 20 ~ dig_bsa * .5, TRUE ~ dig_bsa * 1),
            dig_bsa = case_when(version == "V2" ~ dig_bsa / 100,
                                TRUE ~ dig_bsa)
        )
}

main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
    ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread()
    
    #' retrieve table and recalculate coverage based on version
    all_pso_draw_table <- get_table(PSORIASIS_DRAW_TBL_ID) %>%
        calculate_coverage() %>%
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        dplyr::arrange(desc(createdOn)) %>%
        dplyr::select(-coverage) %>%
        dplyr::mutate(error = NA)
    
    #' get data joinnable with ppacman
    pso_draw_table <- all_pso_draw_table %>%
        join_with_ppacman(visit_ref, ppacman) %>% 
        dplyr::select(recordId, 
                      participantId, 
                      createdOn, 
                      visit_num, 
                      ends_with("coverage"),
                      dig_bsa) %>% 
        dplyr::group_by(participantId, visit_num) %>%
        dplyr::summarise_all(last)
    
    #' get error logging for removed records
    error_log <- log_removed_data(all_pso_draw_table, pso_draw_table)
    
    #' save pso draw dable
    entity <- save_to_synapse(
        data = pso_draw_table,
        output_filename = OUTPUT_REF$feature$output,
        parent = OUTPUT_REF$feature$parent,
        name = "get psodraw features",
        executed = GIT_URL,
        used = PSORIASIS_DRAW_TBL_ID)
    #' set annotations
    synSetAnnotations(
        entity$properties$id,
        analysisType = "psoriasis draw",
        pipelineStep = "feature extraction",
        task = "psoriasis draw")
    
    #' save pso draw error rlog
    entity <- save_to_synapse(
        data = error_log,
        output_filename = OUTPUT_REF$log$output,
        parent = OUTPUT_REF$log$parent,
        name = "get psodraw features - logs",
        executed = GIT_URL,
        used = PSORIASIS_DRAW_TBL_ID)    #' set annotations
    synSetAnnotations(
        entity$properties$id,
        analysisType = "psoriasis draw",
        pipelineStep = "removed data log",
        task = "psoriasis draw")
}

log_process(main(), SCRIPT_PATH)

