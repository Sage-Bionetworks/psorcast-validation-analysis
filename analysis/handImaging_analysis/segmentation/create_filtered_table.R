library(tidyverse)
library(data.table)
library(synapser)

synapser::synLogin()

ANNOTATION <- "syn26477935"
TABLE_ID <- "syn26050060"

store_schema <- function(table_name, project_id) {
    cols <- list(
        Column(name = "recordId", columnType = "STRING", maximumSize = 50),
        Column(name = "finger_key", columnType = "STRING", maximumSize = 20),
        Column(name = "createdOn", columnType = "DATE"),
        Column(name = "participantId", columnType = "STRING"),
        Column(name = "finger_segments", columnType = "FILEHANDLEID"))
    newSchema <- synStore(Schema(name = table_name, 
                                 columns = cols, 
                                 parent = project_id))
    return(newSchema$properties$id)
}

annotation <- synGet(ANNOTATION)$path %>% 
    fread()  %>%
    dplyr::filter(
        nail_pso_status == TRUE | nail_pso_status == FALSE | nail_pso_status == "Unsure") %>%
    dplyr::select(-matches("annot|createdOn|participantId|fileColumnName|nail_pso_status"))
table_entity <- synTableQuery(glue::glue(
    "SELECT * FROM {TABLE_ID}"))
table_data  <- table_entity$asDataFrame() %>% 
    dplyr::inner_join(annotation, by = c("recordId", "finger_key")) %>%
    dplyr::select(-matches("ROW"))

syn_id <- store_schema(
    table_name = "Nail-Segmentation (Curated)",
    project_id = "syn22276946")

newTable <- synStore(Table(syn_id, table_data))

