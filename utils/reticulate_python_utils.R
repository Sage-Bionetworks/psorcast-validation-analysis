#' wrapper function to get synapse table in reticulate
#' into tidy-format (pivot based on filecolumns if exist)
#' @param syn synapse object
#' @param synapse_tbl tbl id in synapse
#' @param file_columns file_columns to download
get_table <- function(syn, synapse_tbl, 
                      file_columns = NULL){
    # get table entity
    entity <- syn$tableQuery(glue::glue("SELECT * FROM {synapse_tbl}"))
    
    # shape table
    table <- entity$asDataFrame() %>%
        tibble::as_tibble(.) %>%
        dplyr::mutate(
            createdOn = as.POSIXct(createdOn/1000, 
                                   origin="1970-01-01"))
    
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
        result <- syn$downloadTableColumns(
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

save_to_synapse <- function(syn,
                            synapseclient,
                            data, 
                            output_filename, 
                            parent,
                            provenance = FALSE, ...){
    data %>% 
        readr::write_tsv(output_filename)
    file <- synapseclient$File(output_filename, parent =  parent)
    
    if(provenance){
        provenance_param = list(...)
        activity = synapseclient$Activity(
            name = provenance_param$name,
            used = provenance_param$used,
            executed = provenance_param$executed)
        
    }else{
        activity = synapseclient$Activity()
    }
    store_to_synapse <- syn$store(file, activity = activity)
    unlink(output_filename)
}
