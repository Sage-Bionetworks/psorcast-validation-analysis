library(githubr)

#' Utility function to join data with PPACMAN assessor
#' by parsing single-visit participant by joining by 'participantId'
#' and multiple-visit participant by their visit reference
#' 
#' @param data dataframe/tibble of participantId, createdOn, ... 
#' @param visit_ref visit reference table id
#' @param ppacman ppacman assessor table id
#' 
#' @return data with joined ppacman assessor
join_with_ppacman <- function(data, 
                              visit_ref, 
                              ppacman, 
                              join_keys = NULL,
                              visit_offset = lubridate::ddays(7)){
    visit_ref <- visit_ref %>%
        dplyr::mutate(participantId = tolower(participantId)) 
    ppacman <- ppacman %>%
        dplyr::mutate(participantId = tolower(participantId)) %>% 
        dplyr::mutate(createdOn = lubridate::as_datetime(createdOn))
    data <- data %>%
        dplyr::mutate(participantId = tolower(participantId)) %>% 
        dplyr::mutate(createdOn = lubridate::as_datetime(createdOn))
    
    if(is.null(join_keys)){
        join_keys <- c("participantId")
    }
    
    #' only visiting once
    single_visit_user <- visit_ref %>% 
        dplyr::filter(!has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' visit multiple times
    multiple_visit_user <- visit_ref %>% 
        dplyr::filter(has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' join multiple visits and single-visits based on precondition
    visit_data <- list(
        single_visit = data %>% 
            dplyr::filter(
                participantId %in% single_visit_user) %>%
            dplyr::inner_join(
                ppacman, 
                by = join_keys) %>%
            dplyr::mutate(
                createdOn = createdOn.y),
        multiple_visit = data %>% 
            dplyr::filter(
                participantId %in% multiple_visit_user) %>%
            dplyr::full_join(
                visit_ref, by = "participantId") %>%
            dplyr::filter(
                createdOn >= (min_createdOn - visit_offset)  & 
                    createdOn <= (max_createdOn + visit_offset)) %>%
            dplyr::inner_join(
                ppacman, 
                by = c(join_keys, "visit_num")) %>%
            dplyr::mutate(
                createdOn = min_createdOn)) %>% 
        bind_rows() %>%
        dplyr::select(-createdOn.y, 
                      -createdOn.x, 
                      -min_createdOn,
                      -max_createdOn,
                      -has_multiple_visit)
    return(visit_data)
}

#' Function to parse in summary.json into tidy-format
#' 
#' @param data dataframe/tibble containing recordId and filePath for each summary.json
#' 
#' @return flattened summary.json row-binded in tidy/long data format
flatten_joint_summary <- function(data){
    purrr::map2_dfr(data$recordId, 
                    data$filePath, 
                    function(curr_record, filePath){
                        tryCatch({
                            data <- jsonlite::fromJSON(filePath)
                            if("simpleJoints" %in% names(data)){
                                identifier_data <- data$simpleJoints
                            }else{
                                identifier_data <- data$selectedIdentifiers
                            }
                            
                            if(nrow(identifier_data) == 0){
                                stop()
                            }
                            identifier_data %>% 
                                tibble::as_tibble() %>%
                                dplyr::mutate(recordId = curr_record)
                        }, error = function(e){
                            tibble(recordId = curr_record,
                                   isSelected = FALSE,
                                   error = geterrmessage())
                        }
                        )
                    }) %>% 
        dplyr::inner_join(data, by = c("recordId"))
}

#' function to get synapse table in reticulate
#' into tidy-format (pivot based on filecolumns if exist)
#' @param synapse_tbl tbl id in synapse
#' @param file_columns file_columns to download
get_table <- function(synapse_tbl, 
                      file_columns = NULL){
    # get table entity
    entity <- synTableQuery(glue::glue("SELECT * FROM {synapse_tbl}"))
    
    # shape table
    table <- entity$asDataFrame() %>%
        tibble::as_tibble(.)
    
    # download all table columns
    if(!is.null(file_columns)){
        table <-  table %>%
            tidyr::pivot_longer(
                cols = all_of(file_columns), 
                names_to = "fileColumnName", 
                values_to = "fileHandleId") %>%
            dplyr::mutate(across(everything(), unlist)) %>%
            dplyr::mutate(fileHandleId = as.character(fileHandleId)) %>%
            dplyr::filter(!is.na(fileHandleId)) %>%
            dplyr::group_by(recordId, fileColumnName) %>% 
            dplyr::summarise_all(last) %>% 
            dplyr::ungroup()
        result <- synDownloadTableColumns(
            table = entity, 
            columns = file_columns) %>%
            tibble::enframe(.) %>%
            tidyr::unnest(value) %>%
            dplyr::select(
                fileHandleId = name, 
                filePath = value) %>%
            dplyr::mutate(filePath = unlist(filePath)) %>%
            dplyr::right_join(table, by = c("fileHandleId"))
    }else{
        result <- table
    }
    return(result)
}

#' Function to search for gyroscope and accelerometer 
#' sensors in a time-series 
#' 
#' @param ts timeseries dataframe of mpower_v2 format (x,y,z,sensortype,timestamp)
#' 
#' @return list of named dataframe for accel and gyro
search_gyro_accel <- function(ts){
    #' split to list
    ts_list <- list()
    ts_list$acceleration <- ts %>% 
        dplyr::filter(
            stringr::str_detect(tolower(sensorType), "^accel"))
    ts_list$rotation <- ts %>% 
        dplyr::filter(
            stringr::str_detect(tolower(sensorType), "^rotation|^gyro"))
    
    #' parse rotation rate only
    if(length(ts_list$rotation$sensorType %>% unique(.)) > 1){
        ts_list$rotation <- ts_list$rotation %>% 
            dplyr::filter(!stringr::str_detect(tolower(sensorType), "^gyro"))
    }
    
    ts_list <- ts_list %>%
        purrr::map(., ~(.x %>%
                            dplyr::mutate(t = timestamp - .$timestamp[1]) %>%
                            dplyr::select(t,x,y,z)))
    return(ts_list)
}
 