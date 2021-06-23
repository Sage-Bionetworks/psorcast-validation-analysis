#' Wrapper function used to get column from
#' to check whether if table exist using
#' synapser::synFindEntityId
#' 
#' @param table_name name of the synapse table
#' @param parent_id the parent id to search table
#' @return table ID if exist, NULL if not
check_synapse_table_existence <- function(tbl_name, parent_id){
    tryCatch({
        synapser::synFindEntityId(tbl_name, parent_id)
    }, error = function(e){
        NULL
    })
}

#' Wrapper function used to get column from
#' source synapse table using synapser::synGetColumns
#' and subset based on parameter (all columns if null)
#' 
#' @param table_id synapse table id
#' @param cols_subset columns to subset from Synapse Table
#' @return Synapse Column Object
get_source_column_schema <- function(table_id, cols_subset = NULL){
    col_obj <- synapser::synGetColumns(table_id)$asList()
    if(is.null(cols_subset)){
        return(col_obj)
    }else{
        col_obj <- purrr::map(
            col_obj,
            function(col_obj){
                if(col_obj$name %in% cols_subset){
                    return(col_obj)}}) %>%
            purrr::reduce(c)
        return(col_obj)
    }
}

#' Wrapper function used to copy file handles using
#' synapserutils::copyFileHandles
#' 
#' @param data dataframe, tibble for Synapse table
#' @param tbl_id source table id for filehandle
#' @param file_handle_cols column for files
#' @param pivot_idx column to pivot metadata
#' @return tibble with new mapped filehandles
copy_file_handles <- function(data, 
                              tbl_id, 
                              file_handle_cols,
                              pivot_idx = NULL){
    cat("\n copying table filehandles")
    if(is.null(pivot_idx)){
        pivot_idx <- data %>% 
            dplyr::select(-all_of(file_handle_cols)) %>% 
            names(.)
    }
    pivoted_data <- data %>%
        tidyr::pivot_longer(all_of(file_handle_cols), 
                            names_to = "file_column_name", 
                            values_to = "file_handle_id") %>%
        dplyr::mutate(associateObjectTypes = "TableEntity",
                      associateObjectIds = tbl_id,
                      contentTypes = "json",
                      fileNames = NA)
    cleaned_data <- pivoted_data %>% 
        tidyr::drop_na(file_handle_id) %>% 
        dplyr::distinct(file_handle_id, .keep_all = TRUE)
    new_filehandle_map <- synapserutils::copyFileHandles(
        fileHandles = cleaned_data$file_handle_id,
        associateObjectTypes = cleaned_data$associateObjectTypes,
        associateObjectIds = cleaned_data$associateObjectIds,
        contentTypes = cleaned_data$contentTypes,
        fileNames = cleaned_data$fileNames) %>% 
        purrr::map_dfr(., function(fhandle){
            tibble::tibble(
                new_file_handle_id = fhandle$newFileHandle$id,
                original_file_handle_id = fhandle$originalFileHandleId)
        })
    result <- pivoted_data %>% 
        dplyr::left_join(
            new_filehandle_map, 
            by = c("file_handle_id" = "original_file_handle_id")) %>%
        tidyr::pivot_wider(
            id_cols = all_of(pivot_idx),
            names_from = "file_column_name", 
            values_from = "new_file_handle_id")
    return(result)
}

#' Function used to regenerate Synapse table
#' from source by using curated data and schema 
#' that will be based on source Synapse table
#' 
#' @param data dataframe, tibble for Synapse table
#' @param tbl_name Synapse table name
#' @param parent what is the parent id
#' @param src_tbl_id source Synapse table ID
#' @param file_handle_cols optional: file handle id to store if exist
#' @param keep_cols optional: what columns to keep, if null then use all
#' @param new_cols optional: what new columns to store in table
#' @param copy_files optional: whether to do copy filehandles here
#' @param provenance optional: whether to use provenance
#' @param ... optional: extra parameter used for provenance
#' @return
regenerate_table <- function(data, 
                             tbl_name, 
                             parent,
                             src_tbl_id,
                             file_handle_cols = NULL,
                             keep_cols = NULL,
                             new_cols = NULL,
                             copy_files = FALSE,
                             provenance = FALSE,
                             ...){
    # check if table if exist (if yes, delete rows)
    cat("check table existence")
    id <- check_synapse_table_existence(tbl_name, parent)
    
    # check if id is not null make table
    if(!is.null(id)){
        cat("\ntable exist in project")
        cat("\ndeleting table rows and schema")
        schema <- synapser::synGet(id)
        if(schema$has_columns()){
            synapser::synGetColumns(
                schema$properties$id)$asList() %>% 
                purrr::walk(function(col){
                    schema$removeColumn(col)
                })
            synStore(schema)
        }
    }
    
    # rebuild column from source
    cat("\nget schema from source table")
    col_obj <- get_source_column_schema(
        table_id = src_tbl_id,
        cols_subset =  c(keep_cols, file_handle_cols)) %>% 
        c(new_cols, .)
    
    # add system sleep time interval to let table update
    Sys.sleep(15)
    
    # create new schema
    create_schema <- Schema(
        name = tbl_name,
        columns = col_obj,
        parent = parent)
    store_new_schema <- synStore(create_schema)
    col_used_in_schema <- col_obj %>% 
        purrr::map(~.x$name) %>% 
        purrr::reduce(c)
    table_id <- store_new_schema$properties$id
    
    # copy file handles when prompted
    if(copy_files){
        data <- copy_file_handles(
            data = data,
            tbl_id = src_tbl_id,
            file_handle_cols = file_handle_cols)
    }
    
    # set provenance 
    if(provenance){
        activity <- Activity(...)
        synSetProvenance(schema$properties$id, activity)
    }
    # match data with schema
    data <- data %>%
        dplyr::select(all_of(col_used_in_schema))
    
    # store to synapse
    cat("\nstoring to synapse...")
    table <- Table(table_id, data)
    synStore(table)
    cat("\ntable stored in Synapse...")
}
