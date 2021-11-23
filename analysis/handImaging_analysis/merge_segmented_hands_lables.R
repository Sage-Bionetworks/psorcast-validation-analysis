######################################################
# Script to create data for segmented images
# psoriasis mapping on each fingers
#
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
TABLE_ID <- "syn26050060"

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
query_clause <- glue::glue("select * FROM {TABLE_ID}")
entity <- synTableQuery(query_clause)
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
                      join_keys = c("participantId", "finger_key")) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(image_name = basename(file_path),
                  participantId = ifelse(
                      stringr::str_detect(
                      participantId, "site"), 
                      toupper(participantId), participantId)) %>%
    dplyr::select(-file_path) %>%
    dplyr::select(recordId, createdOn, everything())

result %>% 
    readr::write_tsv("segmented_fingers_metadata_mapping.tsv")
