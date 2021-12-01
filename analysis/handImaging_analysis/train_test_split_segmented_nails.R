######################################################
# - Script to create folders for segmented nails data
# into training/testing split folders
#
# - Output data corresponding to training/testing
# set into a dataframe 
#
# @maintainer: aryton.tediarjo@sagebase.org
######################################################
library(synapser)
library(data.table)
library(tidyverse)
library(tidymodels)
source("utils/feature_extraction_utils.R")
synapser::synLogin()

# Global Variables
FOLDER_OUTPUT <- "analysis/handImaging_analysis"
VISIT_REF_ID <- "syn25825626"
PPACMAN_ID <- "syn22337133"
ANNOTATION <- "syn26251785"
ANNOTATORS_MAPPING <- list(
    "dan" = "syn26251785",
    "rebecca" = "syn26226989"
)


get_annotations_data <- function(id_list){
    purrr::map(names(id_list), 
               function(user){
                   id <- id_list[[user]]
                   synGet(id)$path %>% 
                       fread() %>%
                       dplyr::select(everything(), 
                                     !!sym(user) := nail_pso_status) %>%
                       dplyr::select(-annotator)}) %>%
        purrr::reduce(dplyr::inner_join, by = c("recordId", 
                                                "createdOn",
                                                "finger_key", 
                                                "participantId")) %>%
        dplyr::select(all_of(
            c("recordId", "createdOn", "finger_key", "participantId")),
            dan, rebecca)  %>%
        map_finger_loc() %>%
        tibble::as_tibble() 
    
}

#' Function to reshape finger location 
#' in PPACMAN data to be tidy format
reshape_finger_loc <- function(data){
    data %>%
        dplyr::mutate(L1 = if_else(stringr::str_detect(finger_nail, "L1"), 1, 0),
                      L2 = if_else(stringr::str_detect(finger_nail, "L2"), 1, 0),
                      L3 = if_else(stringr::str_detect(finger_nail, "L3"), 1, 0),
                      L4 = if_else(stringr::str_detect(finger_nail, "L4"), 1, 0),
                      L5 = if_else(stringr::str_detect(finger_nail, "L5"), 1, 0),
                      R1 = if_else(stringr::str_detect(finger_nail, "R1"), 1, 0),
                      R2 = if_else(stringr::str_detect(finger_nail, "R2"), 1, 0),
                      R3 = if_else(stringr::str_detect(finger_nail, "R3"), 1, 0),
                      R4 = if_else(stringr::str_detect(finger_nail, "R4"), 1, 0),
                      R5 = if_else(stringr::str_detect(finger_nail, "R5"), 1, 0)) %>%
        tidyr::pivot_longer(cols = all_of(c("L1", "L2", "L3", "L4", "L5",
                                            "R1", "R2", "R3", "R4", "R5")),
                            names_to = "finger_key",
                            values_to = "reported_psoriasis")
}

#' Function to re-map app annotations
#' Finger keys
map_finger_loc <- function(data){
    data %>%
        dplyr::mutate(finger_key = case_when(
            finger_key == "left_thumb" ~ "L1",
            finger_key == "left_index" ~ "L2",
            finger_key == "left_middle" ~ "L3",
            finger_key == "left_ring" ~ "L4",
            finger_key == "left_pinky" ~ "L5",
            finger_key == "right_thumb" ~ "R1",
            finger_key == "right_index" ~ "R2",
            finger_key == "right_middle" ~ "R3",
            finger_key == "right_ring" ~ "R4",
            finger_key == "right_pinky" ~ "R5",
            TRUE ~ NA_character_
        ))
}

# retrieve table from Synapse
entity <- synTableQuery(
    "select * from syn26050060")
files <- synDownloadTableColumns(
    entity, "finger_segments") %>% 
    enframe(.) %>%
    dplyr::select(
        finger_segments = name, 
        file_path = value) 

# get visit reference
visit_ref <- synGet(VISIT_REF_ID)$path %>% 
    data.table::fread() %>% 
    tibble::as_tibble()
# get ppacman table
ppacman <- synGet(PPACMAN_ID)$path %>% 
    fread(quote = "") %>% 
    tibble::as_tibble() %>%
    reshape_finger_loc() %>%
    dplyr::select(participantId, 
                  createdOn, 
                  visit_num, 
                  finger_key, 
                  reported_psoriasis)


# get annotations data
# annot_data <- get_annotations_data(ANNOTATORS_MAPPING)

# merge everything to table
result <- entity$asDataFrame() %>%
    dplyr::select(
        recordId,
        createdOn, 
        participantId, 
        finger_key,
        finger_segments) %>%
    dplyr::mutate(participantId = tolower(participantId)) %>%
    dplyr::inner_join(files, on = "finger_segments") %>%
    map_finger_loc() %>%
    tibble::as_tibble() %>%
    dplyr::select(recordId, 
                  participantId, 
                  createdOn,
                  finger_key, 
                  file_path) %>%
    join_with_ppacman(visit_ref, 
                      ppacman,  
                      join_keys = c("participantId", "finger_key"))

# join annotations data with ppacman to get
# visit number reference and in-clinic info
result <- join_with_ppacman(result, 
                            visit_ref, 
                            ppacman,  
                            join_keys = c("participantId", "finger_key"))

# get data removed after joining
removed <- annot_data %>% 
    dplyr::anti_join(result, by = c("recordId")) %>%
    dplyr::mutate(reported_psoriasis = NA, visit_num = NA)

# re-bind removed rows to keep in training data
result <- result %>% 
    dplyr::bind_rows(removed) %>%
    dplyr::select(everything(), 
                  gs_pso = reported_psoriasis) %>%
    dplyr::mutate(gs_pso = as.character(ifelse(gs_pso == 1, TRUE, FALSE))) %>%
    tibble::as_tibble()

 %>%
    dplyr::inner_join(ppacman, by = c("recordId", "finger_key")) %>%
    dplyr::select(recordId,
                  createdOn,
                  finger_key, 
                  participantId, 
                  visit_num, everything())

## testing on gold standard only
set.seed(100)
testing_cases <- result %>%
    dplyr::filter(gs_pso == TRUE)
testing_controls1 <- result %>%
    dplyr::anti_join(testing_cases, by = c("recordId", "finger_key")) %>%
    dplyr::filter(gs_pso == FALSE, 
                  participantId %in% testing_cases$participantId)
testing <- dplyr::bind_rows(testing_cases, testing_controls1)

# get more controls with gold standard annotation
set.seed(100)
chosen_controls <- result %>%
    dplyr::anti_join(testing, by = c("recordId", "finger_key")) %>%
    dplyr::filter(gs_pso == FALSE) %>% 
    dplyr::group_by(participantId) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::sample_n(3) %>% 
    .$participantId
testing_controls2 <- result %>%
    dplyr::filter(participantId %in% chosen_controls)
testing <- dplyr::bind_rows(testing, testing_controls2)


## training based on the rest
training <- result %>%
    dplyr::anti_join(testing, by = c("recordId", "finger_key")) %>%
    dplyr::filter(rebecca == FALSE | rebecca == TRUE)

controls_output = file.path(FOLDER_OUTPUT, "training", "no_reported_psoriasis")
cases_output = file.path(FOLDER_OUTPUT, "training", "reported_psoriasis")

unlink(controls_output, recursive = T)
unlink(cases_output, recursive = T)
dir.create(controls_output, recursive = T)
dir.create(cases_output, recursive = T)

purrr::walk2(
    training$file_path, 
    training$rebecca, 
             function(file_path, reported_psoriasis){
                 if(reported_psoriasis == TRUE){
                     file.copy(file_path, cases_output)
                 }else{
                     file.copy(file_path, controls_output)
                 }
             })


controls_output = file.path(FOLDER_OUTPUT, "testing", "no_reported_psoriasis")
cases_output = file.path(FOLDER_OUTPUT, "testing", "reported_psoriasis")
unlink(controls_output,  recursive = T)
unlink(cases_output,  recursive = T)
dir.create(controls_output, recursive = T)
dir.create(cases_output, recursive = T)
purrr::walk2(testing$file_path, 
             testing$gs_pso, 
             function(file_path, reported_psoriasis){
                 if(reported_psoriasis == TRUE){
                     file.copy(file_path, cases_output)
                 }else{
                     file.copy(file_path, controls_output)
                 }
             })


# Save training/testing reference to Synapse
training %>%
    dplyr::mutate(is_holdout = FALSE) %>%
    dplyr::bind_rows(testing %>% dplyr::mutate(is_holdout = TRUE)) %>%
    dplyr::select(-file_path) %>%
    dplyr::select(recordId, finger_key, visit_num, createdOn, everything()) %>%
    readr::write_tsv("annotation_comparisons.tsv")
file <- synapser::File("annotation_comparisons.tsv", 
                       parent = "syn26250301")
synStore(file)

