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

