library(tidyverse)
library(data.table)
library(synapser)

synapser::synLogin()

FOLDER_ANNOTATED <- "syn25837496"
PROJECT_ID <- "syn22276946"
REF <- list(
    segmented_fingers = list(
        foder_id = "syn26063499",
        table_name = "Finger-Segmentation"),
    segmented_nails = list(
        folder_id = "syn25999657",
        table_name = "Nail-Segmentation"
    )
)

parse_image_name <- function(image_name){
    s <- stringr::str_split(
        image_name, "_") %>%
        unlist()
    if(stringr::str_detect(tolower(image_name), "site")){
        paste(s[[3]], s[[4]], s[[5]], s[[6]], s[[7]], sep = "_")
    }else{
        paste(s[[3]], s[[4]], s[[5]], s[[6]], sep = "_")
    }
}

parse_finger_loc <- function(image_name){
    s <- stringr::str_split(
        image_name, "_") %>%
        unlist()
    paste(s[[1]], s[[2]], sep = "_")
}

get_folder_contents <- function(folder_id){
    synGetChildren(folder_id)$asList() %>% 
        purrr::map_dfr(~.x) %>% 
        dplyr::rowwise() %>%
        dplyr::ungroup()
}

get_annotations <-  function(syn_id){
    syn_id %>% 
        synGetAnnotations() %>%
        map_dfr(~.x[[1]])
}

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

check_for_files <- function(data, 
                            table_id, 
                            project_id, 
                            target_col){
    # upload files
    files <- data[[target_col]] %>%
        purrr::map(
            function(x){
                synapser::synUploadFileHandle(
                    x, project_id)
            }
        )
    
    # get filehandle id mapping
    filehandleid_mapping <- files %>% 
        purrr::map_dfr(function(x){
            tibble::tibble(
                name = x$fileName,
                !!sym(target_col) := x$id)
        })
    
    # update table rows if new files exist
    if(nrow(filehandleid_mapping) > 0){
        newData <- data %>%
            dplyr::select(-!!sym(target_col)) %>%
            dplyr::inner_join(filehandleid_mapping, by = c("name")) %>% 
            dplyr::select(-name)
    }else{
        newData <- data %>% 
            dplyr::mutate(!!sym(target_col) := NA_character_) %>%
            dplyr::select(-name, -path)
    }
    return(list(newData = newData, table_id = table_id))
}

annotated_images <- get_folder_contents(FOLDER_ANNOTATED) %>%
    dplyr::mutate(annotations = purrr::map(id, get_annotations)) %>% 
    dplyr::select(-createdOn) %>%
    tidyr::unnest(annotations) %>%
    dplyr::select(image_name = name,
                  recordId,
                  createdOn,
                  participantId)

image_mapping <- synapserutils::syncFromSynapse(
    synGet(REF$segmented_nails$folder_id)) %>% 
    purrr::map_dfr(function(x){
        tibble(name = x$properties$name,
               path = x$path)})  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(image_name = parse_image_name(name),
                  finger_loc = parse_finger_loc(name)) %>%
    dplyr::ungroup()

# get new table
table <- image_mapping %>% 
    dplyr::left_join(annotated_images, by = c("image_name")) %>%
    dplyr::select(recordId, 
                  finger_key = finger_loc, 
                  createdOn, 
                  participantId,
                  finger_segments = path,
                  name)
table_map <- table %>% 
    update_synapse_table(
        table_name = REF$segmented_nails$table_name,
        project_id = PROJECT_ID,
        join_keys = c("recordId", "finger_key"))

table_map <- check_for_files(
    data = table_map$newData,
    table_id = table_map$table_id,
    project_id = PROJECT_ID,
    target_col = "finger_segments")

newTable <- synStore(Table(
    table_map$table_id, 
    table_map$newData))
