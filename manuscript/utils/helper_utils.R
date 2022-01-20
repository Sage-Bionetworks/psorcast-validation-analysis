#' Function to retrieve github url using githubr
#' @param git_token_path token path
#' @param git_repo github repository
#' @param script_path script path
#' @param ... parameter from getPermLink
get_github_url <- function(git_token_path, 
                           git_repo,
                           script_path,
                           ...){
    setGithubToken(readLines(git_token_path))
    githubr::getPermlink(
        git_repo, 
        repositoryPath = script_path,
        ...)
}

#' Function to log removed data after data processing
#' @param all_data all data before processing
#' @param subset_data data after processing
#' @return data after anti join process
log_removed_data <- function(all_data, subset_data){
    all_data %>% 
        dplyr::filter(!recordId %in% 
                          unique(subset_data$recordId)) %>% 
        dplyr::group_by(recordId) %>% 
        dplyr::summarise_all(last) %>% 
        dplyr::select(recordId, error) %>% 
        dplyr::mutate(error = ifelse(
            is.na(error), 
            "removed from ppacman joining", 
            error))
}


#' utility function to save
#' dataframe to .tsv file to
#' Synapse
#' @param data dataframe
#' @param output_filename output filename
#' @param parent parent id in synapse
save_to_synapse <- function(data, 
                            output_filename, 
                            parent,
                            ...){
    data %>% 
        readr::write_tsv(output_filename)
    file <- File(output_filename, parent =  parent)
    entity <- synStore(file, activity = Activity(...))
    unlink(output_filename)
    return(entity)
}


#' Function to log data processes
#' @param main_function processes wrapped in main
#' @param script_name name of the script
#' @return write to logs/error.log if error and log to
#' logs/pipeline.log for each step of the process
log_process <- function(main_function, script_name){
    tryCatch({
        #' create logger for pipeline
        sink('logs/pipeline.log', append = TRUE)
        cat(paste0(
            "[",Sys.time(), "]", " Running ", script_name), "\n\n")
        sink()
        
        #' run script
        main_function
        
        #' store logger
        sink('logs/pipeline.log', append = TRUE)
        cat(paste0("[",Sys.time(), "]", " Done Running ", script_name), "\n\n")
        sink()
    }, error = function(e) {
        #' store script error to pipeline.log
        sink('logs/pipeline.log', append = TRUE)
        cat(paste0("[",Sys.time(), "] ", script_name, " Error occured: check logs/error.log"), "\n\n")
        sink()
        
        #' store error to error.log
        sink("logs/error.log")
        cat(paste0("[",Sys.time(), "] ", script_name, " - ", e), "\n\n")
        sink()
        stop("Stopped due to error - Please check error.log")
    })
    
}

