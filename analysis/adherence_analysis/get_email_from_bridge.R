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

MONTH <- lubridate::month(lubridate::now())
TABLE_ID <- "syn26445447"
PARENT_ID <- "syn26438179"
OUTPUT_FILE <- "healthcode_metadata_mapping_from_bridge.tsv"
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
            tibble::tibble(error = e$message)
        })
    })
}

#' Filter based on month using pivot as 
#' it automatically ignores the NA
#' @param data the activity adherence data (tidy)
#' @param month the month of when data is queried on
#' @return filtered data
filter_based_on_month <- function(data, month){
    data %>%
        pivot_longer(cols = !all_of(c("healthCode", "weekInStudy"))) %>%
        dplyr::filter(lubridate::month(value) == month) %>%
        pivot_wider(id_cols = all_of(c("healthCode", "weekInStudy")),
                    names_from = name,
                    values_from = value)
}

main <- function(){
    git_url <- get_github_url(
        git_token_path = config::get("git")$token_path,
        git_repo = config::get("git")$repo,
        script_path = SCRIPT_PATH
    )
    
    tbl_entity <- synTableQuery(glue::glue("SELECT * FROM {TABLE_ID}"))
    data <- tbl_entity$asDataFrame() %>%
        tibble::as_tibble() %>%
        dplyr::select(-starts_with("ROW")) %>%
        filter_based_on_month(month = MONTH) %>%
        get_info_mapping_bridge() %>%
        save_to_synapse(output_filename = OUTPUT_FILE,
                        parent = PARENT_ID,
                        executed = git_url,
                        description = "extract info from Bridge",
                        used = TABLE_ID)
}

main()
    