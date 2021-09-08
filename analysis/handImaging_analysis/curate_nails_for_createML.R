library(synapser)
library(data.table)
library(tidyverse)
synapser::synLogin()

source("utils/feature_extraction_utils.R")

FOLDER_OUTPUT <- "analysis/handImaging_analysis"
VISIT_REF_ID <- "syn25825626"
PPACMAN_ID <- "syn22337133"

#' Utility function to join data with PPACMAN assessor
#' by parsing single-visit participant by joining by 'participantId'
#' and multiple-visit participant by their visit reference
#' 
#' @param data dataframe/tibble of participantId, createdOn, ... 
#' @param visit_ref_tbl visit reference table id
#' @param ppacman_tbl ppacman assessor table id
#' 
#' @return data with joined ppacman assessor
join_with_ppacman <- function(data, visit_ref_tbl, ppacman_tbl, join_keys){
    #' only visiting once
    single_visit_user <- visit_ref_tbl %>% 
        dplyr::filter(!has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' visit multiple times
    multiple_visit_user <- visit_ref_tbl %>% 
        dplyr::filter(has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' join multiple visits and single-visits based on precondition
    visit_data <- list(
        single_visit = data %>% 
            dplyr::filter(participantId %in% single_visit_user) %>%
            dplyr::inner_join(ppacman_tbl, by = join_keys) %>%
            dplyr::mutate(createdOn = createdOn.y),
        multiple_visit = data %>% 
            dplyr::filter(participantId %in% multiple_visit_user) %>%
            dplyr::full_join(visit_ref_tbl, by = "participantId") %>%
            dplyr::filter(createdOn >= min_createdOn & 
                              createdOn <= max_createdOn) %>%
            dplyr::inner_join(ppacman_tbl, by = c(join_keys, "visit_num")) %>%
            dplyr::mutate(createdOn = min_createdOn)) %>% 
        bind_rows() %>%
        dplyr::select(-createdOn.y, 
                      -createdOn.x, 
                      -min_createdOn,
                      -max_createdOn,
                      -has_multiple_visit)
    return(visit_data)
}

visit_ref <- synGet(VISIT_REF_ID)$path %>% 
    data.table::fread() %>% 
    tibble::as_tibble()
ppacman <- synGet(PPACMAN_ID)$path %>% 
    fread(quote = "") %>% 
    tibble::as_tibble() %>%
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
                        values_to = "reported_psoriasis") %>%
    dplyr::select(participantId, createdOn, visit_num, finger_key, reported_psoriasis)

entity <- synTableQuery("select * from syn26050060")

files <- synDownloadTableColumns(entity, "finger_segments") %>% 
    enframe(.) %>%
    dplyr::select(
        finger_segments = name, 
        file_path = value)

data <- synTableQuery(
    "select * from syn26050060")$asDataFrame() %>%
    dplyr::mutate(
        which_hand = if_else(
            stringr::str_detect(finger_key, "left"), "L", "R"),
        finger_number = case_when(
            stringr::str_detect(finger_key, "thumb") ~ 1,
            stringr::str_detect(finger_key, "index") ~ 2,
            stringr::str_detect(finger_key, "middle") ~ 3,
            stringr::str_detect(finger_key, "ring") ~ 4,
            TRUE ~ 5),
        finger_key = str_c(which_hand, finger_number)) %>%
    dplyr::select(createdOn, 
                  participantId, 
                  finger_key,
                  finger_segments) %>%
    dplyr::inner_join(files, on = "finger_segments") %>%
    join_with_ppacman(visit_ref, ppacman, c("participantId", "finger_key")) %>%
    tibble::as_tibble()

train_test_split <- initial_split(data, 
                                  prop = 1/2, 
                                  strata = reported_psoriasis)

controls_output = file.path(FOLDER_OUTPUT, "training", "no_reported_psoriasis")
cases_output = file.path(FOLDER_OUTPUT, "training", "reported_psoriasis")

unlink(controls_output)
unlink(cases_output)
dir.create(controls_output)
dir.create(cases_output)

purrr::walk2(training(train_test_split)$file_path, 
             training(train_test_split)$reported_psoriasis, 
             function(file_path, reported_psoriasis){
    if(reported_psoriasis == 1){
        file.copy(file_path, cases_output)
    }else{
        file.copy(file_path, controls_output)
    }
})


controls_output = file.path(FOLDER_OUTPUT, "testing", "no_reported_psoriasis")
cases_output = file.path(FOLDER_OUTPUT, "testing", "reported_psoriasis")

unlink(controls_output)
unlink(cases_output)
dir.create(controls_output)
dir.create(cases_output)

purrr::walk2(testing(train_test_split)$file_path, 
             testing(train_test_split)$reported_psoriasis, 
             function(file_path, reported_psoriasis){
                 if(reported_psoriasis == 1){
                     file.copy(file_path, cases_output)
                 }else{
                     file.copy(file_path, controls_output)
                 }
             })
