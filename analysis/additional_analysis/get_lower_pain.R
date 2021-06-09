############################################################
#' This script will be used for summarizing reported each 
#' joint counts for Psorcast users 
#' 
#' Note: it will exclude finger joints and only use
#' main joint location (ankle, hip, elbow, wrist etc.)
#' 
#' @author: aryton.tediarjo@sagebase.org
#' @maintainer: aryton.tediarjo@sagebase.org
############################################################
rm(list=ls())
gc()

# import libraries
library(synapser)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(jsonlite)
library(githubr)
source("utils/feature_extraction_utils.R")
synLogin()

WALK_TBL <- 'syn22281384'
DIG_JCOUNTS_TBL <- "syn25830490"
MD_JCOUNTS_TBL <- "syn25830491"
JOINT_LOCATION <- c("knee", "hip", "ankle", 
                    "wrist", "elbow", "shoulder")

walk_tbl <- synTableQuery(glue::glue("SELECT * FROM {WALK_TBL}"))$asDataFrame() %>%
    join_with_ppacman(visit_ref_tbl_id = "syn25825626", 
                      ppacman_tbl_id = "syn25006883")
dig_jc <- synGet(DIG_JCOUNTS_TBL)$path %>% fread() %>%
    join_with_ppacman(visit_ref_tbl_id = "syn25825626", 
                      ppacman_tbl_id = "syn25006883")
md_jc <- synGet(MD_JCOUNTS_TBL)$path %>% fread() %>%
    join_with_ppacman(visit_ref_tbl_id = "syn25825626", 
                      ppacman_tbl_id = "syn25006883")

annotated_recordId <- walk_tbl %>% 
    dplyr::inner_join(dig_jc, by = c("participantId", "visit_num"))  %>%
    dplyr::mutate(age = coalesce(age.x, age.y),
                  sex = coalesce(sex.x, sex.y),
                  diagnosis = coalesce(diagnosis.x, diagnosis.y),
                  recordId = coalesce(recordId.x, recordId.y)) %>%
    dplyr::mutate(diagnosis = 
                      str_extract(tolower(diagnosis), 
                                  "psa|pso|control")) %>%
    tidyr::pivot_longer(cols = matches("status"), 
                 names_to = "identifier", 
                 values_to = "status") %>% 
    dplyr::select(recordId, identifier, status) %>%
    dplyr::mutate(has_lower_pain = case_when(stringr::str_detect(
        identifier, str_c(JOINT_LOCATION, collapse = "|")) & 
            !is.na(status) ~ 1, TRUE ~ 0)) %>% 
    group_by(recordId) %>%
    dplyr::summarise(
        sum_lower_pain = sum(has_lower_pain),
        has_lower_pain = max(has_lower_pain))

output_file <- "lower_pain_recordIds.tsv"
annotated_recordId %>% readr::write_tsv(output_file)
file <- synapser::File(output_file, parent = "syn25830737")
synStore(file)
unlink(output_file)



