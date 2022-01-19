########################################
#' Code to retrieve activity adherence
#' by mapping each activity into columns
#' and respective createdOn
#' 
#' @maintainer: aryton.tediarjo@sagebase.org
##########################################
library(synapser)
library(tidyverse)
library(data.table)
source("utils/feature_extraction_utils.R")

synapser::synLogin()

TABLE_NAME <- "PsorcastAdherence"
PROJECT_ID <- "syn22276946"
DESCRIPTION <- "Measure adherence from psorcast project"
SCRIPT_PATH <- "analysis/adherence_analysis/get_activity_adherence_table.R"

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
            dplyr::mutate(dataGroups = replace_na(dataGroups, "")) %>%
            dplyr::filter(!stringr::str_detect(dataGroups, "test_user")) %>%
            dplyr::select(recordId, 
                          dataGroups,
                          createdOn, 
                          healthCode, 
                          filePath, 
                          tableName)
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
        #' handle duplicate by extracting the first occurence
        dplyr::group_by(recordId,
                        dataGroups,
                        createdOn, 
                        healthCode, 
                        tableName) %>% 
        dplyr::summarise(weekInStudy = first(weekInStudy),
                         startDate = first(startDate)) %>% 
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
                                              "weekInStudy",
                                              "dataGroups",
                                              "startDate")),
                           values_fill = NA_character_,
                           values_fn = max)
}

#' Store schema object to synapse
#' 
#' @param table_name desired table name
#' @param project_id project to store table in
store_schema <- function(table_name, project_id){
    cols <- list(
        Column(name = "healthCode", columnType = "STRING", maximumSize = 50),
        Column(name = "startDate", columnType = "DATE"),
        Column(name = "dataGroups", columnType = "STRING"),
        Column(name = "weekInStudy", columnType = "INTEGER"),
        Column(name = "HandImaging-v2", columnType = "DATE"),
        Column(name = "PsoriasisDraw-v4", columnType = "DATE"),
        Column(name = "PsoriasisAreaPhoto-v2", columnType = "DATE"),
        Column(name = "FootImaging-v2", columnType = "DATE"),
        Column(name = "JointCounting-v2", columnType = "DATE"),
        Column(name = "Walk30Seconds-v2", columnType = "DATE"),
        Column(name = "DigitalJarOpen-v3", columnType = "DATE"))
    newSchema <- synStore(Schema(name = table_name, 
                                 columns = cols, 
                                 parent = project_id))
    return(newSchema$properties$id)
}

#' Update Synapse Table, by indexing new rows based on 
#' joining keys
#' 
#' @param data new data to store
#' @param table_name table name
#' @param project_id the project id to store table in
#' @param join_keys the join keys to index data
#' 
#' @return a list of new data, table_id
update_synapse_table <- function(data, table_name, project_id, join_keys){
    table_id <- synFindEntityId(table_name, project_id)
    if(!is.null(table_id)){
        synapse_tbl <- synTableQuery(
            glue::glue("SELECT * FROM {table_id}"))$asDataFrame() %>% 
            dplyr::select(-matches("ROW"))
        data <- data %>% 
            dplyr::anti_join(synapse_tbl, by = join_keys)
    }else{
        table_id <- store_schema(table_name, project_id)
    }
    
    return(list(newData = data, table_id = table_id))
}

main <- function(){
    #' get github token
    git_url <- get_github_url(
        git_token_path = config::get("git")$token_path,
        git_repo = config::get("git")$repo,
        script_path = SCRIPT_PATH,
        ref="branch", 
        refName='main'
    )
    
    #' get all activity tables (row-binded)
    data <- get_activity_tables() %>% 
        parse_study_state() %>%
        pivot_activity_tbl()
    
    #' get table mapping to get new rows
    table_map <- update_synapse_table(
        data, 
        TABLE_NAME, 
        PROJECT_ID, 
        join_keys = c("healthCode", "weekInStudy"))
    
    #' store table to synapse
    newTable <- synStore(Table(
        table_map$table_id, 
        table_map$newData))
    
    #' set provenance
    activity = Activity(used = TABLE_LIST %>% unname() %>% unlist(),
                        executed = git_url)
    synSetProvenance(table_map$table_id, activity = activity)
}

main()
