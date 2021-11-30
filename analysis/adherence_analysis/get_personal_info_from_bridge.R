########################################
#' Code to fetch emails from bridge
#' using healthcode from Synapse Tables
#' 
#' @maintainer: aryton.tediarjo@sagebase.org
##########################################
library(tidyverse)
library(data.table)
library(synapser)
library(bridgeclient)
library(githubr)
source("utils/feature_extraction_utils.R")

synapser::synLogin()
bridgeclient::bridge_login(
    study = "sage-psorcast",
    credentials_file = ".bridge_creds")

YEAR <- lubridate::year(lubridate::now())
MONTH <- lubridate::month(lubridate::now())
MONTH_NAME <- month.name[MONTH]
OUTPUT_FILE <- glue::glue("Psorcast_National_Launch",
                          YEAR, 
                          "_", 
                          MONTH_NAME, 
                          "_", "
                          incentives_info.tsv")
ACTIVITY_THRESHOLD <- 3

TABLE_ID <- "syn26445447"
PARENT_ID <- "syn26438179"
SCRIPT_PATH <- "analysis/adherence_analysis/get_email_from_bridge.R"


#' Helper functiton to get info mapping from Bridge
#' @param data the activity adherence data (tidy)
#' @return Bridge mapping (heathCode, email, phoneInfo)
get_info_mapping_bridge <- function(data){
    purrr::map_dfr(data$healthCode, function(hc){
        tryCatch({
            fetch_data <- bridgeclient::get_participant(health_code = hc)
            tibble::tibble(
                email = fetch_data$attributes$compensateEmail, 
                healthCode = fetch_data$healthCode,
                phone = fetch_data$phone$number)
        }, error = function(e){
            tibble::tibble(error = e$message, healthCode = hc)
        })
    })
}

#' Filter based on month using pivot as 
#' it automatically ignores the NA
#' @param data the activity adherence data (tidy)
#' @param month the month of when data is queried on
#' @return filtered data
filter_by_timestamps <- function(data, month, year){
    data %>%
        dplyr::filter(
            lubridate::month(value) == month,
            lubridate::year(value) == year)
}


filter_by_adherence <- function(data, threshold){
    data %>% 
        dplyr::group_by(healthCode) %>%
        dplyr::summarise(n = n_distinct(weekInStudy, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n >= threshold)
}

main <- function(){
    git_url <- get_github_url(
        git_token_path = config::get("git")$token_path,
        git_repo = config::get("git")$repo,
        script_path = SCRIPT_PATH,
        ref="branch", 
        refName='main'
    )
    tbl_entity <- synTableQuery(glue::glue("SELECT * FROM {TABLE_ID}"))
    data <- tbl_entity$asDataFrame() %>%
        tibble::as_tibble() %>%
        dplyr::select(-starts_with("ROW")) %>%
        pivot_longer(cols = !all_of(c("healthCode", "weekInStudy"))) %>%
        filter_by_timestamps(MONTH, YEAR) %>%
        filter_by_adherence(ACTIVITY_THRESHOLD) %>% 
        dplyr::distinct(healthCode) %>%
        get_info_mapping_bridge() %>%
        save_to_synapse(
            output_filename = OUTPUT_FILE,
            parent = PARENT_ID,
            executed = git_url,
            description = "extract info from Bridge",
                        used = TABLE_ID)
}

main()
    