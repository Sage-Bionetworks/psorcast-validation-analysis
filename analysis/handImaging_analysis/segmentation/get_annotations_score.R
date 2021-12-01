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
SCRIPT_PATH <- "analysis/handImaging_analysis/merge_segmented_hands_labels.R"
VISIT_REF_ID <- "syn25825626"
PPACMAN_ID <- "syn22337133"
TABLE_ID <- "syn26479618"
PARENT_ID <- "syn22342373"
ANNOT_FOLDER_ID <- "syn26133849"
METADATA_MAPPING <- "syn26477252"
IMAGE_FOLDER_ID <- "syn25999657"

GIT_URL <- get_github_url(
    git_token_path = config::get("git")$token_path,
    git_repo = config::get("git")$repo, 
    script_path = SCRIPT_PATH,
    ref="branch", 
    refName='main'
)

get_image_locations <- function(folder_id){
    synGetChildren(folder_id)$asList() %>% 
        purrr::map_dfr(function(x){x}) %>% 
        dplyr::select(synId = id, image_name = name)
}

get_heatmap <- function(data){
    data %>% 
        tidyr::drop_na(nail_pso_status,
                       reported_psoriasis) %>%
        dplyr::mutate(annot = factor(nail_pso_status, levels = c(0,1)),
                      gs = factor(reported_psoriasis, levels = c(0,1))) %>%
        yardstick::conf_mat(annot,gs, dnn = c("Annotation", "In-Clinic"))
}

get_annotations <- function(){
    ids <- synGetChildren(ANNOT_FOLDER_ID)$asList() %>%
        purrr::map_dfr(~.x) %>% 
        dplyr::filter(!stringr::str_detect(name, "_old")) %>% .$id
    purrr::map_dfr(ids, function(x){
        entity <- synapser::synGet(x)
        annotator_prefix <- entity$properties$name %>%
            stringr::str_split("_") %>% unlist() %>% .[[1]]
        data <- fread(entity$path) %>% 
            dplyr::mutate(annotator = annotator_prefix)
        if(nrow(data) == 0){
            return(tibble::tibble())
        }else{
            return(data)
        }
    })
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

metadata_mapping <- synGet(METADATA_MAPPING)$path %>% fread()
annotations <- get_annotations() %>%
    map_finger_loc() %>%
    dplyr::inner_join(metadata_mapping, by = c("recordId", "finger_key")) %>%
    dplyr::mutate(nail_pso_status = ifelse(nail_pso_status, 1, 0)) %>%
    dplyr::select(annotator, 
                  recordId, 
                  finger_key, 
                  nail_pso_status,
                  image_name,
                  reported_psoriasis) %>%
    dplyr::inner_join(get_image_locations(IMAGE_FOLDER_ID)) %>%
    dplyr::group_by(annotator) %>%
    tidyr::nest() %>%
    dplyr::mutate(plot = purrr::map(data, get_heatmap))

conf_mat <- map2(annotations$annotator, 
     annotations$plot, function(a, p){
         p + labs(title = a)}) %>%
    patchwork::wrap_plots()
ggsave("concordance_matrix.png", conf_mat, width = 10, height = 4)

output_filename <- "concordance_matrix.png"
entity <- synGet("syn26484969")
content <- paste0("${image?fileName=", 
                  output_filename, 
                  "&align=none&scale=50}")
wiki = Wiki(title = "concordance matrix",
            owner = entity,
            markdown = content,
            attachments = list(output_filename))
synStore(wiki)



store_to_synapse <- map2(annotations$annotator, 
                         annotations$data, function(a, d){
                             filename <- glue::glue("{a}_merged_nails_annotations.tsv")
                             d %>% 
                                 dplyr::select(recordId, 
                                               finger_key, 
                                               annot = nail_pso_status,
                                               gs = reported_psoriasis,
                                               everything()) %>%
                                 readr::write_tsv(filename)
                             file <- synapser::File(filename, parent = "syn26484969")
                             synStore(file)})
