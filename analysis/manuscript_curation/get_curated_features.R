########################################################
#' Description: Script to curate features 
#' for manuscript data for figures
#' 
#'  Activities curated:
#'  -  Walk30Secs
#'  - Digital Jar Opener
#' 
#' Maintainer: aryton.tediarjo@sagebase.org
########################################################

library(synapser)
library(tidyverse)
library(data.table)
library(githubr)

synapser::synLogin()


############################
# Global Vars
############################
MERGED_FEATURES <- "syn25832975"
PPACMAN_DATA <- "syn25006883"
PARENT_ID <- "syn25704998"
OUTPUT_FILENAME <- list(
    djo = "djo_curated_features.tsv",
    walk = "walk30s_curated_features.tsv"
)

############################
# Git Reference
############################
SCRIPT_PATH <- file.path('analysis', 
                         "manuscript_curation", 
                         "get_curated_features.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- githubr::getPermlink(
    repository = githubr::getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)


#' Function  to annotate each classes based
#' on pain label
#' @param data 
annotate_classes <- function(data){
    data %>%
        dplyr::mutate(upper_body_pain = gs_upper_body_pain,
                      lower_body_pain = ifelse(has_lower_pain == 1, TRUE, FALSE),
                      upper_enthesis =  ifelse(has_upper_enthesitis == 1, TRUE, FALSE),
                      lower_enthesis =  ifelse(has_lower_enthesitis == 1, TRUE, FALSE)) %>% 
        dplyr::mutate(upper_body_pain = factor(upper_body_pain, level = c(FALSE, TRUE)),
                      lower_body_pain = factor(lower_body_pain, level = c(FALSE, TRUE)),
                      upper_enthesis = factor(upper_enthesis, level = c(FALSE, TRUE)),
                      lower_enthesis = factor(lower_enthesis, level = c(FALSE, TRUE)))
}

#' Function to retrieve ppacman dataset
#' @param data 
get_ppacman <- function(){
    synTableQuery(
        glue::glue("SELECT *  FROM {PPACMAN_DATA}"))$asDataFrame() %>% 
        dplyr::select(participantId, 
                      visit_num = `Visit Number`, 
                      ent_loc =`Enthesitis Label`) %>%
        dplyr::mutate(
            has_lower_enthesitis = ifelse(str_detect(ent_loc, "knee|achilles"), 1, 0),
            has_upper_enthesitis = ifelse(str_detect(ent_loc, "arm"), 1, 0))
}

#' Function to retrieve rotation data
#' and total rotation features
#' @param data 
#' @return featurized rotation data
get_total_rotation <- function(data){
    data %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(total_rotation = sum(
            djo_leftCounter, djo_rightCounter,
            djo_leftClockwise, djo_rightClockwise))
}


#' Function to retrieve walk30s data
#' and total rotation features
#' @param data 
#' @return featurized walk data
get_walk_features <- function(data){
    remove_features <- c("y_speed_of_gait", "x_speed_of_gait", 
                         "z_speed_of_gait", "AA_stride_regularity",
                         "AA_step_regularity", "AA_symmetry") %>% 
        stringr::str_c(collapse = "|")
    data %>% 
        dplyr::select(-matches(remove_features)) 
}

#' get uppper body pain
#' from merged features
#' @param data merged dataframe with required columns
#' @return data with upper body pain class
get_upper_body_pain <- function(data){
    data %>%
        dplyr::rowwise() %>% 
        dplyr::mutate(dig_upper_body_pain = 
                          !(is.na(dig_jc_status_wrist) & 
                                is.na(dig_jc_status_elbow) & 
                                is.na(dig_jc_status_shoulder))) %>%
        dplyr::mutate(gs_upper_body_pain = 
                          !(is.na(gs_jc_status_wrist) & 
                                is.na(gs_jc_status_elbow) & 
                                is.na(gs_jc_status_shoulder))) %>%
        dplyr::mutate(gs_upper_body_swell = 
                          !(is.na(gs_swell_status_wrist) & 
                                is.na(gs_swell_status_elbow) & 
                                is.na(gs_swell_status_shoulder)))
}

#' get lower body pain
#' from merged features
#' @param data merged dataframe with required columns
#' @return data with lower body pain class
get_lower_body_pain <- function(data){
    data %>%
        dplyr::mutate(
            complete_case_joint_pain_list = gs_jc_joint_list,
            complete_case_swollen_joint_list = gs_swell_joint_list) %>%
        dplyr::mutate(
            complete_case_joint_pain_list = coalesce(
                complete_case_joint_pain_list,  tjc_backup),
            complete_case_swollen_joint_list = coalesce(
                complete_case_swollen_joint_list,  sjc_backup)) %>%
        dplyr::mutate(
            has_lower_pain = case_when(
                stringr::str_detect(
                    complete_case_joint_pain_list, 
                    "knee|hip|ankle") ~ 1, TRUE ~ 0),
            knee_pain = case_when(
                stringr::str_detect(
                    complete_case_joint_pain_list, "knee") ~ 1, TRUE ~  0),
            hip_pain = case_when(
                stringr::str_detect(
                    complete_case_joint_pain_list, "hip") ~ 1, TRUE ~  0),
            ankle_pain = case_when(
                stringr::str_detect(
                    complete_case_joint_pain_list, "ankle") ~ 1, TRUE ~ 0)) %>%
        dplyr::ungroup()
}

main <- function(){
    #' Get merged features and merge with PPACMAN information
    #' and annotate classes based on outcome variables
    merged_features <- fread(
        synapser::synGet(MERGED_FEATURES)$path,
        quote = "") %>%
        dplyr::left_join(
            get_ppacman(), 
            by = c("participantId", "visit_num")) %>%
        get_upper_body_pain() %>%
        get_lower_body_pain() %>%
        annotate_classes()
    
    #' Get DjO features 
    djo_features <- merged_features %>% 
        get_total_rotation() %>%
        dplyr::mutate(
            combined_upper_pain = as.logical(upper_enthesis) | 
                as.logical(upper_body_pain)) %>%
        dplyr::select(participantId, 
                      visit_num, 
                      age,
                      sex,
                      diagnosis,
                      upper_enthesis,
                      upper_body_pain, 
                      combined_upper_pain,
                      matches("djo|total_rotation")) %>%
        readr::write_tsv(OUTPUT_FILENAME$djo)
    
    #' Get walk30secs features 
    walk_features <- merged_features %>% 
        dplyr::mutate(
            combined_lower_pain = as.logical(lower_enthesis) | 
                as.logical(lower_body_pain)) %>%
        dplyr::select(participantId, 
                      visit_num,
                      age,
                      sex,
                      diagnosis,
                      lower_enthesis,
                      lower_body_pain, 
                      combined_lower_pain,
                      matches("^x_|^y_|^z_|^AA_")) %>%
        readr::write_tsv(OUTPUT_FILENAME$walk)
    
    #' Create activity object
    activity <- synapser::Activity(used = MERGED_FEATURES,
                                   executed = GIT_URL)
    
    #' Save Digital Jar Opener Features to Synapse
    file <- synapser::File(
        OUTPUT_FILENAME$djo, 
        parent = PARENT_ID)
    synStore(file, activity = activity)
    unlink(file$path)
    
    #' Save Walk30Secs Features to Synapse
    file <- synapser::File(
        OUTPUT_FILENAME$walk, 
        parent = PARENT_ID)
    synStore(file, activity = activity)
    unlink(file$path)
}

main()


