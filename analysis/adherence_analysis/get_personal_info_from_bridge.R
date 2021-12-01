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
MONTH <- 11
MONTH_NAME <- month.name[MONTH]
DATE <- lubridate::ymd(glue::glue("{YEAR}-{MONTH}-01"))
MONTH_CEILING <- lubridate::ceiling_date(DATE, unit = 'month') - 
    lubridate::ddays(x = 1)



OUTPUT_FILE <- glue::glue("psorcast_",
                          YEAR, 
                          "_", 
                          MONTH_NAME, 
                          "_", 
                          "incentives_participants.tsv")
ACTIVITY_THRESHOLD <- 3

TABLE_ID <- "syn26486970"
PARENT_ID <- "syn26438179"
SCRIPT_PATH <- "analysis/adherence_analysis/get_personal_info_from_bridge.R"


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

get_n_weeks_adherence <- function(data){
    data %>% 
        tidyr::drop_na(value) %>%
        dplyr::group_by(healthCode, startDate) %>%
        dplyr::summarise(n_week_adherence = 
                             n_distinct(weekInStudy, na.rm = T),
                         last_weekInStudy = max(weekInStudy)) %>% 
        dplyr::ungroup()
}

get_n_days_enrolled <- function(data, date){
    data %>%
        dplyr::mutate(n_days_enrolled = round(difftime(date, startDate), 0)) %>%
        dplyr::filter(value <= date | is.na(value)) %>% 
        dplyr::group_by(healthCode, startDate) %>% 
        dplyr::summarise(n_days_enrolled = max(n_days_enrolled),
                         last_weekInStudy = max(weekInStudy)) %>% 
        dplyr::ungroup()
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
        pivot_longer(cols = !all_of(c("healthCode", "startDate","weekInStudy")))
    
    result <- list(
        n_days_enrolled =  data %>% 
            get_n_days_enrolled(MONTH_CEILING),
        n_weeks_adherence =  data %>% 
            get_n_weeks_adherence()) %>% 
        purrr::reduce(dplyr::full_join, 
                      by = c("healthCode", "startDate","last_weekInStudy")) %>% 
        dplyr::mutate(is_adherence = ifelse(n_days_enrolled < lubridate::ddays(30) | 
                                                n_week_adherence > 3, TRUE, FALSE))
    
    mapping <- result %>% 
        distinct(healthCode) %>%
        get_info_mapping_bridge()
    
    result %>% 
        dplyr::inner_join(mapping, by = c("healthCode")) %>% 
        save_to_synapse(
            output_filename = OUTPUT_FILE,
            parent = PARENT_ID,
            executed = git_url,
            description = "extract info from Bridge",
                        used = TABLE_ID)
    unlink(OUTPUT_FILE)
}

main()
    