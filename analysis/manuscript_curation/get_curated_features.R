library(synapser)
library(tidyverse)
library(data.table)

synapser::synLogin()

MERGED_FEATURES <- "syn25832975"
PPACMAN_DATA <- "syn25006883"
PARENT_ID <- "syn25704998"
OUTPUT_FILENAME <- list(
    djo = "djo_curated_features.tsv",
    walk = "walk30s_curated_features.tsv",
)

data <-  fread(synGet(MERGED_FEATURES)$path)


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

get_total_rotation <- function(data){
    data %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(total_rotation = sum(
            djo_leftCounter, djo_rightCounter,
            djo_leftClockwise, djo_rightClockwise)) 
}

get_walk_features <- function(data){
    data %>% 
        drop_na(c("x_step_duration_md",
                  "x_step_duration_iqr",
                  "x_stride_deviation_md",
                  "x_stride_deviation_iqr",
                  "x_cadence_md",
                  "x_cadence_iqr",
                  "x_symmetry_md",
                  "x_symmetry_iqr")) %>%
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
                    complete_case_joint_pain_list, "ankle") ~ 1, TRUE ~ 0))
}

filter_features <- function(data){
    data %>% 
        dplyr::select(
            participantId, 
            visit_num, 
            age,
            sex,
            matches(
                "djo|pain|x_step|x_stride|x_symmetry|x_cadence|enthesitis|total_rotation|diagnosis"))
}

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


features <- data %>%
    dplyr::left_join(
        get_ppacman(), 
        by = c("participantId", "visit_num")) %>%
    get_total_rotation() %>%
    get_walk_features() %>%
    get_upper_body_pain() %>%
    filter_features() %>%
    annotate_classes()


djo_features <- all_features %>% 
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
                  matches("djo|rotation")) %>%
    drop_na() %>% 
    readr::write_tsv("djo_features_labelled.tsv")

walk_features <- all_features %>% 
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
                  matches("^x_")) %>%
    drop_na() %>% 
    readr::write_tsv("walk_features_labelled.tsv")


file <- synapser::File(
    OUTPUT_FILENAME$djo, 
    parent = PARENT_ID)
synStore(file)
unlink(file$path)

file <- synapser::File(
    OUTPUT_FILENAME$walk, 
    parent = PARENT_ID)
synStore(file)
unlink(file$path)

