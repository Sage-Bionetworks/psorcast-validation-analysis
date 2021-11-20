#################################################################
#' Description:
#' This script is used to merge plaque data with ppacman 
#' gold standard data. Criteria of data being merged is by merging
#' on participant id and createdOn of the ppacman data and each
#' annotations data. Matched by +- 7 days of imaging and ppacman
#' 
#' Author: Aryton Tediarjo (aryton.tediarjo@sagebase.org)
#################################################################

library(synapser)
library(tidyverse)
library(data.table)
library(githubr)

synLogin()

VISIT_TIMESTAMP_REF <- "syn25825626"
ANNOTATIONS_PARENT_ID <- "syn25614357"
MERGED_PARENT_ID <- "syn25614358"
PPACMAN_TBL <- "syn25006883"
OUTPUT_FILENAME <- "merged_gold_standard.tsv"
IMAGE_REF_PARENT_FOLDER <- "syn25837659"
SYNAPSE_URL_HEADER <- "https://www.synapse.org/#!Synapse:"
MERGE_SCRIPT <- "merge_photo_with_ppacman.R"
WIKI_SCRIPT <- "generate_concordance_conf_matrix.R"
GIT_PATH <- "~/git_token.txt"
GIT_REPO <- "arytontediarjo/psorcastValidationAnalysis"
githubr::setGithubToken(readLines(GIT_PATH))
MERGE_GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = file.path('analysis/psoPlaque_analysis', MERGE_SCRIPT))
WIKI_GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = file.path('analysis/psoPlaque_analysis', WIKI_SCRIPT))


##############################
# Outputs
##############################
get_all_annotations_from_folder <- function(){
    synapser::synGetChildren(ANNOTATIONS_PARENT_ID)$asList() %>%
        purrr::map(., function(child){
            entity <- synGet(child$id)
            annot_data <- fread(entity$path) %>% 
                dplyr::mutate(source_id = child$id)}) %>% 
        purrr::reduce(rbind) %>%
        dplyr::select(annot_pga = PGA,
                      annot_erythema = erythema,
                      annot_induration = induration,
                      annot_scaling = scaling,
                      everything())
}

get_ppacman_assessor <- function(){
    synapser::synTableQuery(
        glue::glue("SELECT * FROM {PPACMAN_TBL}"))$asDataFrame() %>% 
        dplyr::select(
            participantId,
            createdOn = `Date`,
            gs_pga = `Gestaltic PGA`,
            gs_erythema = `Bellwether Erythema`,
            gs_induration = `Bellwether Induration`,
            gs_scaling = `Bellwether Scaling`,
            visit_num = `Visit Number`) %>%
        dplyr::arrange(desc(createdOn)) %>%
        dplyr::group_by(participantId, visit_num, createdOn) %>%
        dplyr::summarise_at(
            .vars = c(
                "gs_pga",
                "gs_erythema", 
                "gs_induration", 
                "gs_scaling"), 
            .funs = first) %>%
        dplyr::mutate(createdOnDate = lubridate::as_date(createdOn)) %>%
        tidyr::drop_na() %>%
        dplyr::ungroup()
}

annotations_to_wide_format <- function(synId){
    synapser::synGetAnnotations(synId) %>% 
        tibble::enframe(.) %>% 
        tidyr::unnest(value) %>%
        tidyr::unnest(value) %>% 
        pivot_wider(names_from = name, values_from = value) %>%
        dplyr::mutate(synId = synId)
}

get_image_ref_from_folder_entity <- function(){
    synapser::synGetChildren(IMAGE_REF_PARENT_FOLDER)$asList() %>% 
        purrr::map_dfr(function(child){
            annotations_to_wide_format(child$id) %>%
                dplyr::mutate(image_link = glue::glue(
                    SYNAPSE_URL_HEADER, synId))
        }) %>%
        dplyr::select(recordId, synId, image_link)
}

merge_data <- function(annotations_data, 
                       ppacman_data, 
                       visit_ref_data,
                       folder_image_ref){
    purrr::map(annotations_data$annotator %>% unique(.), 
               function(curr_annotator){
                   image_file <- glue::glue(
                       "{curr_annotator}_{OUTPUT_FILENAME}", 
                        curr_annotator = curr_annotator)
                   annotator_data <- annotations_data %>% 
                       dplyr::filter(annotator == curr_annotator)
                   source_id <- annotator_data$source_id
                   annotator_data %>%
                       join_with_ppacman(ppacman_data, 
                                         visit_ref_data) %>%
                       dplyr::left_join(folder_image_ref, 
                                        by = "recordId") %>% 
                       dplyr::select(
                           recordId, 
                           participantId, 
                           visit_num,
                           matches("erythema"),
                           matches("induration"),
                           matches("scaling"),
                           matches("pga"),
                           annotator,
                           image_link, 
                           synId) %>%
                       readr::write_tsv(image_file)
                   file <- synapser::File(image_file, 
                                          parentId = MERGED_PARENT_ID)
                   synapser::synStore(file, 
                            activityName = "merge annotations",
                            used = source_id,
                            executed = c(MERGE_GIT_URL,
                                         WIKI_GIT_URL))
                   unlink(image_file)
               })
}

main <- function(){
    annotations_data <-  get_all_annotations_from_folder()
    ppacman_data <- get_ppacman_assessor()
    visit_ref_data <- fread(synGet(VISIT_TIMESTAMP_REF)$path)
    folder_image_ref <- get_image_ref_from_folder_entity()
    
    merge_data(
        annotations_data, 
        ppacman_data, 
        visit_ref_data,
        folder_image_ref)
    
    source("analysis/psoPlaque_analysis/generate_concordance_conf_matrix.R")
}

main()

