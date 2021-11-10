library(synapser)
library(tidyverse)
library(data.table)
source("utils/feature_extraction_utils.R")

synapser::synLogin()

PROJECT_ID <- "syn21586352"
PARENT <- "syn22276946"
OUTPUT_FILENAME <- "psorcast_launch_activity_adherence.tsv"
DESCRIPTION <- "Measure adherence from psorcast project"

TABLE_LIST <- list(
    "PsoriasisDraw-v4" = "syn26428811",
    "FootImaging-v2" = "syn26428822",
    "JointCounting-v2" = "syn26428820",
    "HandImaging-v2" = "syn26428821",
    "DigitalJarOpen-v3" = "syn26428816",
    "Walk30Seconds-v2" = "syn26428823",
    "PsoriasisAreaPhoto-v2" = "syn26428818")

#' Function to flatten json filepaths
#' @param filepath to read into dataframe
flatten_json <- function(file_path){
    data <- tryCatch({
        jsonlite::fromJSON(file_path)
    }, error = function(e){
        tibble::tibble(
            error = e$message)
    })
    return(data)
}

#' Function to parse each activity tables
#' and download each of the studyStates in each tables
#' and label each table with its table name
#' 
#' @return row-binded dataframe of all Synapse Table
get_activity_tables <- function(){
    purrr::map_dfr(names(TABLE_LIST), function(tbl_name){
        tbl_id <- TABLE_LIST[[tbl_name]]
        tbl_df <- get_table(tbl_id, "studyStates.json")
        tbl_df %>%
            dplyr::mutate(tableName = tbl_name) %>%
            dplyr::select(recordId, createdOn, healthCode, filePath, tableName)
    })
}

#' Function to parse studyState filepaths in dataframe
#' 
#' @param data dataframe cotaining studyState
#' @return dataframe with weekInStudy idenfitifer 
parse_study_state <- function(data){
    data %>%
        dplyr::mutate(studyStates = purrr::map(filePath, flatten_json)) %>%
        tidyr::unnest(studyStates) %>% 
        tidyr::drop_na() %>% 
        dplyr::group_by(recordId, createdOn, healthCode, tableName) %>% 
        dplyr::summarise(weekInStudy = n_distinct(weekInStudy)) %>% 
        dplyr::ungroup()
}

#' Function to pivot table into ClinOps Format
#' index1, index2, sum(activity1), sum(activity2)
#' 
#' @param data data with weekInStudy
#' @return data with pivot wider (melt) of all activities
pivot_activity_tbl <- function(data){
    data %>% 
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        tidyr::pivot_wider(names_from = tableName, 
                           values_from = createdOn,
                           id_cols = all_of(c("healthCode", 
                                              "weekInStudy")),
                           values_fill = NA_character_,
                           values_fn = max)
}


get_columns <- function(){
    cols <- list(
        Column(name = "healthCode", columnType = "STRING", maximumSize = 50),
        Column(name = "weekInStudy", columnType = "INTEGER"),
        Column(name = "HandImaging-v2", columnType = "DATE"),
        Column(name = "PsoriasisDraw-v4", columnType = "DATE"),
        Column(name = "PsoriasisAreaPhoto-v2", columnType = "DATE"),
        Column(name = "FootImaging-v2", columnType = "DATE"),
        Column(name = "JointCounting-v2", columnType = "DATE"),
        Column(name = "Walk30Seconds-v2", columnType = "DATE"),
        Column(name = "DigitalJarOpen-v3", columnType = "DATE"))
    return(cols)
}

data <- get_activity_tables() %>% 
    parse_study_state() %>%
    pivot_activity_tbl()

schema <- Schema(name = "PsorcastAdherence-v1", 
                 columns = get_columns(), 
                 parent = PARENT)
table <- Table(schema, data)
table <- synStore(table)


