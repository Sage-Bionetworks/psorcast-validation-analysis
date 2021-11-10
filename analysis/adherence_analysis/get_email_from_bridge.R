library(tidyverse)
library(data.table)
library(synapser)
library(bridgeclient)

synapser::synLogin()
bridgeclient::bridge_login(
    study = "sage-psorcast",
    credentials_file = ".bridge_creds")

MONTH <- lubridate::month(lubridate::now())
TABLE_ID <- "syn26445447"
OUTPUT_FILE <- "healthcode_metadata_mapping_from_bridge.tsv"

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


tbl_entity <- synTableQuery(glue::glue("SELECT * FROM {TABLE_ID}"))
data <- tbl_entity$asDataFrame() %>%
    tibble::as_tibble() %>%
    dplyr::select(-starts_with("ROW")) %>%
    dplyr::group_by(healthCode, weekInStudy) %>%
    dplyr::filter()
data <- synapser::synGet(FILE_ID)$path %>% 
    fread() %>%
    get_info_mapping_bridge() %>%
    save_to_synapse(output_filename = OUTPUT_FILE,
                    parent = "syn26438179",
                    description = "extract info from Bridge",
                    used = FILE_ID)
    