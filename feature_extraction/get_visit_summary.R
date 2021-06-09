############################################
#' Script is used for summarizing patient visit
#' and their activity. It will also be used
#' to infer their timestamp reference (when
#' the activity is most likely occuring)
#' 
#' @author: Aryton Tediarjo 
#' @author_email: aryton.tediarjo@sagebase.org
#############################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(githubr)
library(data.table)
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
source("utils/feature_extraction_utils.R")
source('utils/processing_log_utils.R')
synapser::synLogin()


####################
# Global Variables
####################
PARENT_SYN_ID <- "syn25825625"
ERROR_LOG_SYN_ID <- "syn25832341"
TIME_THRESH <- lubridate::ddays(30)
TBL_REF <- list(
    ppacman_assessor = 'syn25006883',
    joint_counting = 'syn22281786',
    md_joint_counting = 'syn22281781',
    md_joint_swelling = 'syn22281780',
    foot_imaging = 'syn22281750',
    hand_imaging = 'syn22281749',
    area_photo = 'syn22281748',
    jar_opener = 'syn22281747',
    psoriasis_draw = 'syn22281746',
    walk_30s = 'syn22281384')
OUTPUT_REF <- list(
    summary = "psorcast_summary_table.tsv",
    visit_timestamp_reference = "visit_timestamp_reference.tsv",
    removed_participant = "removed_participant_visit_timestamp_reference.tsv"
)

############################
# Git Reference
############################
SCRIPT_PATH <- file.path('feature_extraction', "get_visit_summary.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)

########################
## get curated tables
########################
get_participant_multiple_visits <- function(data){
    data %>% 
        dplyr::filter(visit_num > 1) %>%
        .$participantId %>%
        unique(.)
}

#' function to get psorcast tables
#' with information of location, what is the visit number (if exist)
get_psorcast_tables <- function(tbl_id){
    tbl_object <- synapser::synTableQuery(glue::glue("SELECT * FROM {tbl_id}"))
    tbl_entity <- synapser::synGet(tbl_id)
    tbl_name <- tbl_entity$properties$name
    tbl_id <- tbl_entity$properties$id
    tbl_data <- tbl_object$asDataFrame() %>%
        dplyr::mutate(visit_num = NA)
    
    if(tbl_name == "PPACMAN_assessor_data-Curated"){
        tbl_data <- tbl_data %>% 
            dplyr::select(
                participantId,
                createdOn = Date, 
                visit_num = "Visit Number")
    }
    tbl_data %>%
        dplyr::mutate(
            source = tbl_name,
            id = tbl_id) %>% 
        dplyr::select(
            any_of(c("participantId", 
                     "createdOn", 
                     "visit_num", 
                     "source",
                     "id")))
}

get_single_visit_timestamp_ref <- function(data){
    data <- data %>% dplyr::mutate(visit_num = 1)
    all_data <- data %>%
        dplyr::distinct(participantId, visit_num)
    summarized_data <- data %>%
        dplyr::group_by(participantId) %>%
        dplyr::summarise(
            min_createdOn = min(createdOn, na.rm = T),
            max_createdOn = max(createdOn, na.rm = T))
    all_data %>%
        dplyr::left_join(summarized_data, 
                         by = "participantId") %>%
        dplyr::mutate(has_multiple_visit = FALSE)
}

get_multiple_visit_timestamp_ref <- function(data){
    data %>%
        dplyr::group_by(participantId) %>%
        nest() %>%
        dplyr::mutate(visit_info = purrr::map(data, .f = function(user_info){
            curate_nested_user_multiple_visit(user_info)})) %>%
        tidyr::unnest(visit_info) %>%
        dplyr::mutate(has_multiple_visit = TRUE) %>%
        dplyr::select(participantId, 
                      visit_num, 
                      has_multiple_visit,
                      min_createdOn, 
                      max_createdOn)
}

curate_nested_user_multiple_visit <- function(data){
    data %>%
        dplyr::arrange(createdOn) %>%
        dplyr::mutate(
            diff_up = c(0,diff(createdOn)),
            diff_down = as.numeric(lead(.$createdOn) - .$createdOn)) %>%
        dplyr::mutate(visit_num = case_when((diff_up == 0) ~ 1,
                                            (diff_up > TIME_THRESH) ~ 1,
                                            TRUE ~ 0),
                      visit_num = cumsum(visit_num)) %>%
        dplyr::group_by(visit_num) %>%
        dplyr::summarise(min_createdOn = min(createdOn, na.rm = T),
                         max_createdOn = max(createdOn, na.rm = T)) %>%
        dplyr::ungroup()
}

main <- function(){
    # get all data to tidy-format
    summary_tbl <- TBL_REF %>% 
        purrr::map_dfr(get_psorcast_tables) %>% 
        dplyr::arrange(desc(createdOn))
    
    # get patient with multiple visit
    multiple_visit <- summary_tbl %>%
        dplyr::filter(participantId %in% 
                          get_participant_multiple_visits(.)) %>%
        get_multiple_visit_timestamp_ref()
    
    # get patient with single visit
    single_visit <- summary_tbl %>% 
        dplyr::anti_join(multiple_visit, by = "participantId") %>%
        get_single_visit_timestamp_ref()
    
    # bind single-visit and multiple visit and get range
    visit_data <- bind_rows(single_visit, multiple_visit) %>%
        dplyr::mutate(min_createdOn = as.character(min_createdOn),
                      max_createdOn = as.character(max_createdOn))
    
    #' get removed participant
    participant_removed <- summary_tbl %>% 
        dplyr::anti_join(visit_data, 
                         by = c("participantId")) %>% 
        write_tsv(OUTPUT_REF$removed_participant)
    
    #' save summary table
    save_to_synapse(data = summary_tbl %>%
                        dplyr::mutate(createdOn = as.character(createdOn)),
                    output_filename = OUTPUT_REF$summary, 
                    parent = PARENT_SYN_ID,
                    name = "get row-binded summary table",
                    executed = GIT_URL,
                    used = TBL_REF %>% purrr::reduce(c))
    
    #' save visit reference
    save_to_synapse(data = visit_data,
                    output_filename = OUTPUT_REF$visit_timestamp_reference, 
                    parent = PARENT_SYN_ID,
                    name = "get visit timestamp reference",
                    executed = GIT_URL,
                    used = TBL_REF %>% purrr::reduce(c))
    
    #' save log removed files
    save_to_synapse(data = participant_removed,
                    output_filename = OUTPUT_REF$removed_participant, 
                    parent = ERROR_LOG_SYN_ID,
                    name = "get removed participant",
                    executed = GIT_URL,
                    used = TBL_REF %>% purrr::reduce(c))
}

log_process(main(), SCRIPT_PATH)

