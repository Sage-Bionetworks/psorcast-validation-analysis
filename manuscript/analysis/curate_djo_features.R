########################################################
#' Description: Script to curate features 
#' for manuscript data for figures
#' 
#'  Activities curated:
#'  - Digital Jar Opener
#' 
#' Maintainer: aryton.tediarjo@sagebase.org
########################################################
library(synapser)
library(tidyverse)
library(data.table)
library(githubr)
source("manuscript/utils/fetch_id_utils.R")

synapser::synLogin()


############################
# Global Vars
############################
MERGED_FEATURES <- SYN_ID_REF$feature_extraction$merged
PPACMAN_DATA <- "syn25006883"
PARENT_ID <- SYN_ID_REF$curated_features$parent
OUTPUT_FILENAME <- list(
    djo = "djo_curated_features.tsv"
)

############################
# Git Reference
############################
SCRIPT_PATH <- file.path('manuscript',
                         'analysis',
                         'curate_djo_features.R')
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
                      upper_enthesis =  ifelse(has_upper_enthesitis == 1, TRUE, FALSE)) %>% 
        dplyr::mutate(upper_body_pain = factor(upper_body_pain, level = c(FALSE, TRUE)),
                      upper_enthesis = factor(upper_enthesis, level = c(FALSE, TRUE)))
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
            has_upper_enthesitis = ifelse(str_detect(ent_loc, "arm"), 1, 0)) %>% 
        dplyr::mutate(participantId = tolower(participantId))
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

main <- function(){
    #' Get merged features and merge with PPACMAN information
    #' and annotate classes based on outcome variables
    merged_features <- fread(
        synapser::synGet(MERGED_FEATURES)$path,
        quote = "") %>%
        dplyr::inner_join(
            get_ppacman(), 
            by = c("participantId", "visit_num")) %>%
        get_upper_body_pain() %>%
        annotate_classes() %>% 
        dplyr::group_by(participantId, visit_num) %>%
        dplyr::summarise_all(last)
    
    #' Get DjO features 
    djo_features <- merged_features %>% 
        get_total_rotation() %>%
        dplyr::mutate(
            combined_upper_pain = as.logical(upper_enthesis) | 
                as.logical(upper_body_pain)) %>%
        dplyr::group_by(participantId, 
                        visit_num,
                        age,
                        sex,
                        diagnosis,
                        upper_enthesis,
                        upper_body_pain, 
                        combined_upper_pain) %>%
        dplyr::select(
            matches(
                "^rightCounter|^leftCounter|^rightClockwise|^leftClockwise|^djo|^total_rotation")) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na() %>%
        readr::write_tsv(OUTPUT_FILENAME$djo)
    
    #' Create activity object
    activity <- synapser::Activity(used = MERGED_FEATURES,
                                   executed = GIT_URL,
                                   name = "curate psorcast djo features for analysis",
                                   description = "merge features with clinical endpoints for analysis")
    
    #' Save Digital Jar Opener Features to Synapse
    file <- synapser::File(
        OUTPUT_FILENAME$djo, 
        parent = PARENT_ID)
    entity <- synStore(file, activity = activity)
    unlink(file$path)
    
    #' add annotation
    synSetAnnotations(
        entity$properties$id,
        analysisType = "digital jar open",
        pipelineStep = "feature curation",
        task = "digital jar open")
    
}

main()


