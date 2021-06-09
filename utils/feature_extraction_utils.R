#' Utility function to join data with PPACMAN assessor
#' by parsing single-visit participant by joining by 'participantId'
#' and multiple-visit participant by their visit reference
#' 
#' @param data dataframe/tibble of participantId, createdOn, ... 
#' @param visit_ref_tbl visit reference table id
#' @param ppacman_tbl ppacman assessor table id
#' 
#' @return data with joined ppacman assessor
join_with_ppacman <- function(data, visit_ref_tbl, ppacman_tbl){
    #' only visiting once
    single_visit_user <- visit_ref_tbl %>% 
        dplyr::filter(!has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' visit multiple times
    multiple_visit_user <- visit_ref_tbl %>% 
        dplyr::filter(has_multiple_visit) %>%
        .$participantId %>% unique()
    
    #' join multiple visits and single-visits based on precondition
    visit_data <- list(
        single_visit = data %>% 
            dplyr::filter(participantId %in% single_visit_user) %>%
            dplyr::inner_join(ppacman_tbl, by = c("participantId")) %>%
            dplyr::mutate(createdOn = createdOn.y),
        multiple_visit = data %>% 
            dplyr::filter(participantId %in% multiple_visit_user) %>%
            dplyr::full_join(visit_ref_tbl, by = c("participantId")) %>%
            dplyr::filter(createdOn >= min_createdOn & 
                              createdOn <= max_createdOn) %>%
            dplyr::inner_join(ppacman_tbl, by = c("participantId", "visit_num")) %>%
            dplyr::mutate(createdOn = min_createdOn)) %>% 
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
                            identifier_data <- jsonlite::fromJSON(filePath) %>% 
                                .$selectedIdentifier
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

save_to_synapse <- function(data, 
                            output_filename, 
                            parent,
                            ...){
    data %>% 
        readr::write_tsv(output_filename)
    file <- File(output_filename, parent =  parent)
    store_to_synapse <- synStore(file, activity = Activity(...))
    unlink(output_filename)
}
