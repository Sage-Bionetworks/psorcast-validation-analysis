############################################################
# This script will be used for summarizing reported each 
# joint counts for PSA users
# @author: aryton.tediarjo@sagebase.org
############################################################

# import libraries
library(synapser)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(jsonlite)
synLogin()


############################
# Global Vars
############################
MD_JOINT_COUNT <- "syn22281781"
MD_JOINT_SWELL <- "syn22281780"
PPACMAN_TBL <- "syn25006883"
FILE_COLUMNS <- "summary.json"
OUTPUT_FILE <- "joint_counts_summary.tsv"
SCRIPT_NAME <- "get_joint_summary.R"
GIT_PATH <- "~/git_token.txt"
GIT_REPO <- "arytontediarjo/psorcastValidationAnalysis"

githubr::setGithubToken(readLines(GIT_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = file.path('analysis', SCRIPT_NAME))

#' This function is used to parse ppacman assessor
get_ppacman_assessor <- function(){
    # get entirety of ppacman tbl
    ppacman_tbl<- synapser::synTableQuery(
        glue::glue("SELECT * FROM {PPACMAN_TBL}"))$asDataFrame() %>% 
        dplyr::select(participantId = `Participant ID`,
                      createdOn = `Date`,
                      diagnosis = Diagnosis,
                      visit_num = `Visit Number`) %>% 
        dplyr::group_by(participantId) %>% 
        dplyr::mutate(distinct_visit = n_distinct(visit_num)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            only_one_visit = case_when(
                distinct_visit == 1 ~ TRUE, TRUE ~ FALSE)) %>%
        dplyr::mutate(diagnosis = str_trim(diagnosis),
                      diagnosis = str_remove(diagnosis, ","))
}

flatten_joint_summary <- function(data){
    purrr::map2_dfr(data$recordId, data$filePath, 
                    function(curr_record, filePath){
                        tryCatch({
                            identifier_data <- fromJSON(filePath) %>% 
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

join_with_ppacman <- function(data){
    ppacman_tbl <-  get_ppacman_assessor()
    one_visit <- ppacman_tbl %>% 
        dplyr::filter(distinct_visit == 1)
    multiple_visit <- ppacman_tbl %>% 
        dplyr::filter(distinct_visit > 1)
    
    one_visit_join <- one_visit %>%
        dplyr::left_join(data, by = c("participantId")) %>%
        dplyr::mutate(createdOn = createdOn.x) %>%
        dplyr::select(-createdOn.x, -createdOn.y)
    
    multiple_visit_join <- multiple_visit %>% 
        dplyr::left_join(data, 
                         by = c("participantId")) %>%
        dplyr::mutate(timediff = createdOn.x - createdOn.y,
                      createdOn = createdOn.x) %>%
        dplyr::filter(abs(timediff) <= lubridate::ddays(7)) %>%
        dplyr::select(-timediff, -createdOn.x, -createdOn.y)
    
    rbind(one_visit_join, multiple_visit_join)
}

get_gs_joint_summaries <- function(){
    entity_jc <- synTableQuery(glue::glue(
        "SELECT * FROM {MD_JOINT_COUNT}"))
    entity_js <- synTableQuery(glue::glue(
        "SELECT * FROM {MD_JOINT_SWELL}"))
    tbl <- rbind(entity_jc$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "joint_count"), 
                 entity_js$asDataFrame() %>%
                     dplyr::mutate(fileColumnName = "joint_swell")) %>%
        dplyr::select(
            everything(), 
            fileHandleId = !!sym(FILE_COLUMNS))
    file_data <- list(
        jcount = synDownloadTableColumns(
            entity_jc, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value),
        jswell = synDownloadTableColumns(
            entity_js, FILE_COLUMNS) %>% 
            tibble::enframe() %>%
            dplyr::select(fileHandleId = name, filePath = value)) %>% 
        purrr::reduce(rbind)
    
    tbl %>% 
        dplyr::inner_join(
            file_data, by = c("fileHandleId"))
}

    
result <- get_gs_joint_summaries() %>%
    flatten_joint_summary() %>%
    dplyr::select(participantId = participantID, everything()) %>%
    join_with_ppacman() %>%
    dplyr::filter(diagnosis == "PsA")

detected_psa_users <- (result$participantId %>% unique() %>% length())
summary <- result %>% 
    group_by(identifier) %>% 
    summarise(
        sum_jc = n_distinct(participantId[(fileColumnName == "joint_count") &
                                              (isSelected == TRUE)]),
        sum_js = n_distinct(participantId[(fileColumnName == "joint_swell") &
                                              (isSelected == TRUE)])) %>%
    dplyr::mutate(sum_reported = sum_jc + sum_js,
                  avg_reported = sum_reported/detected_psa_users) %>%
    dplyr::arrange(desc(avg_reported))

summary %>% write_tsv(OUTPUT_FILE)
file <- synapser::File(OUTPUT_FILE, "syn24180371")
synStore(file, 
         activityName = "get each joint location summary",
         used = c(PPACMAN_TBL, MD_JOINT_SWELL, MD_JOINT_COUNT), 
         executed = GIT_URL)
unlink(OUTPUT_FILE)

