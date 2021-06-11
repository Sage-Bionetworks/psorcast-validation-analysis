########################################################################
#' Psoriasis Validation
#' 
#' Purpose: 
#' To rename image files from synapse tables and save that image as file
#' entities and generate summary mapping of image photo as .tsv file
#' 
#' Author: Aryton Tediarjo
#' email: aryton.tediarjo@sagebase.org
#' TODO: just do appends
########################################################################
rm(list=ls())
gc()

##############################
# Libraries
##############################
library(synapser)
library(jsonlite)
library(plyr)
library(tidyverse)
library(purrr)
library(githubr)
library(readr)
library(png)
library(jpeg)
source('utils/processing_log_utils.R')
source('utils/feature_extraction_utils.R')
synapser::synLogin()


##############################
# Global Variables
##############################
OUTPUT_FILE <- "psorcastPhotoTableMapping.tsv"
SYNAPSE_URL_HEADER <- "https://www.synapse.org/#!Synapse:"
SUMMARY_TBL_OUTPUT_PARENT_ID <- "syn25825625"
DATA_REF <- list(
  psorcast_draw = list(
    syn_id = "syn22281746",
    parent = "syn25837658",
    file_columns = c("summaryImage.png", "summaryImage.jpg"),
    marker = "psoDraw"
  ),
  psorcast_area_photo = list(
    syn_id = "syn22281748",
    parent = "syn25837659",
    file_columns = c("psoriasisAreaPhoto.png", "psoriasisAreaPhoto.jpg"),
    marker = "areaPhoto"
  ),
  psorcast_foot_imaging = list(
    syn_id = "syn22281750",
    parent = "syn25837377",
    file_columns = c("summaryImage.jpg"),
    marker = "footImaging"
  ),
  psorcast_left_foot_imaging = list(
    syn_id = "syn22281750",
    parent = "syn25837377",
    file_columns = c("leftFoot.jpg"),
    marker = "leftFootImaging"
  ),
  psorcast_right_foot_imaging = list(
    syn_id = "syn22281750",
    parent = "syn25837377",
    file_columns = c("rightFoot.jpg"),
    marker = "rightFootImaging"
  ),
  psorcast_hand_imaging = list(
    syn_id = "syn22281749",
    parent = "syn25837496",
    file_columns = c("summaryImage.jpg"),
    marker = "handImaging"
  ),
  psorcast_left_hand_imaging = list(
    syn_id = "syn22281749",
    parent = "syn25837496",
    file_columns = c("leftHand.jpg"),
    marker = "leftHandImaging"
  ),
  psorcast_right_hand_imaging = list(
    syn_id = "syn22281749",
    parent = "syn25837496",
    file_columns = c("rightHand.jpg"),
    marker = "rightHandImaging"
  )
)

##############################
# Git Reference
##############################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- file.path('feature_extraction', "copy_site_images.R")
setGithubToken(
  readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='main'), 
    repositoryPath = SCRIPT_PATH)

##############################
# Helpers
##############################

#' helper function to generate mapping summary for imaging data
#' that contains each image paths and their respective Ids and URL
#' 
#' @param data dataframe containing image paths
#' @param output_id output folder for images
#' @return joined table of image paths and information (location, URL) and other metadata
generate_mapping_summary <- function(data, output_id){
  Sys.sleep(5) # add sys sleep to wait for synapse to get updated
  saved_images <- purrr::map_dfr(
    as.list(synapser::synGetChildren(output_id)), function(x){
      return(tibble::tibble(image_filename = x$name, id = x$id))})
  data %>%
    dplyr::mutate(image_filename = basename(path)) %>%
    dplyr::select(
      participant_id = participantId, 
      recordId,
      createdOn, 
      image_filename) %>%
    dplyr::inner_join(., saved_images, by = c("image_filename")) %>% 
    dplyr::mutate(url = paste0(SYNAPSE_URL_HEADER, id),
                  createdOn = as.character(createdOn)) %>%
    tibble::as_tibble()
}

#' wrapper function create manifest dataframe
#' @param data dataframe/tibble
#' @param used provenance param for 'used'
#' @param parent provenance param for 'parent'
#' @param executed provenance param for 'executed'
#' @return
create_manifest <- function(data, used, parent, executed){
  data %>%
    dplyr::mutate(
      parent = parent,
      used = used,
      executed = executed) %>%
    dplyr::select(path = new_path, 
                  parent, 
                  used, 
                  executed, 
                  recordId, 
                  participantId, 
                  createdOn)
}

#' wrapper function to upload manifest using syncToSynapse
#' 
#' @param data dataframe containing manifest.tsv columns
#' @return data
sync_manifest <- function(data){
  get_manifest <- data %>% write_tsv("manifest.tsv")
  sync_to_synapse <- synapserutils::syncToSynapse("manifest.tsv")
  unlink("manifest.tsv")
  return(data)
}

#' Function to convert images to jpeg
#' 
#' @param image_input_path old image filepath
#' @param image_new_path new image filepath
#' @return jpeg image name
convert_png_image <- function(image_input_path, image_new_path){
  jpeg::writeJPEG(png::readPNG(image_input_path), 
                  target = image_new_path, 
                  quality = 100)
}

#' Function to copy filename from synapse table file handle
#' and rename it into desired format
#' 
#' @param data dataframe with mapping of json filepaths
#' @param col name of column containing list of json filepaths
#' @param format the format of the image
#' @param marker additional marker for image name
#' @return map each filepath to a new filename
map_image_to_new_filename <- function(tbl, col, marker){
  ## if image is png
  tbl %>%
    dplyr::filter(!is.na(.[[col]])) %>%
    plyr::ddply(., .variables = c("participantId", 
                                  "createdOn", 
                                  "recordId"), 
                       function(row){
        new_path <- file.path(
          dirname(file.path(row[[col]])), 
          paste0(gsub(" ", "", row[["participantId"]]), "_",
                 marker, "_",
                 gsub(":", ".", 
                  gsub(" ", "_", as.character(row[["createdOn"]]))),
                 ".jpg"))
        row$new_path <- new_path
        #TODO: change this to be more automated
        if(tools::file_ext(row[[col]]) == "png"){
          convert_png_image(row[[col]], new_path)
        }else{
          file.copy(from = row[[col]], to = new_path)
        }
      return(row)})
}


main <- function(){
  #' - parse through each tables
  #' - map each filehandle into desired new filename 
  #' - create 'manifest.tsv' containing filepaths and synapse/provenance info
  #' - sync to synapse using the manifest tsv
  #' - save summary table for each image, its metadata, and location
  all_used_ids <- DATA_REF %>% 
    purrr::map(~.x$syn_id) %>% 
    purrr::reduce(c)
  mapping_result <- purrr::map(names(DATA_REF), function(activity){
    data_filter <- DATA_REF[[activity]]
    get_table(data_filter$syn_id, 
              file_columns = data_filter$file_columns) %>%
      map_image_to_new_filename('filePath', data_filter$marker) %>% 
      create_manifest(
        used = data_filter$syn_id,
        executed = GIT_URL,
        parent = data_filter$parent)}) %>%
    purrr::reduce(bind_rows) %>%
    sync_manifest() %>%
    save_to_synapse(
      data = .,
      output_filename = OUTPUT_FILE,
      parent = SUMMARY_TBL_OUTPUT_PARENT_ID, 
      name = "get summary table from images",
      executed = GIT_URL,
      used = all_used_ids
    )
}

log_process(main(), SCRIPT_PATH)