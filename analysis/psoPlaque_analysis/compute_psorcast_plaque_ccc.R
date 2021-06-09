library(synapser)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggplot2)
synLogin()

OUTPUT_FILENAME <- "psorcast_plaque_ccc_scores.tsv"
MERGED_PARENT_ID <- "syn25614358"
CONCORDANCE_PARENT_ID <- "syn25650752"

SCRIPT_NAME <- "generate_psorcast_plaque_ccc.R"
GIT_PATH <- "~/git_token.txt"
GIT_REPO <- "arytontediarjo/psorcastValidationAnalysis"
githubr::setGithubToken(readLines(GIT_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = file.path('analysis', SCRIPT_NAME))

get_merged_annotation_data <- function(){
    synapser::synGetChildren(MERGED_PARENT_ID)$asList() %>%
        purrr::map(., function(child){
            entity <- synGet(child$id)
            annot_data <- fread(entity$path)}) %>% 
        purrr::reduce(rbind)
}

get_grouped_ccc <- function(data, group, truth, estimate, col){
    data %>% 
        dplyr::group_by(!!sym(group)) %>%
        yardstick::ccc(!!sym(truth), !!sym(estimate)) %>%
        dplyr::mutate(metric = col)
}

run_ccc_scoring <- function(data){
    list(get_grouped_ccc(data, "annotator", "gs_erythema", "annot_erythema", "erythema"),
         get_grouped_ccc(data, "annotator", "gs_scaling", "annot_scaling", "scaling"),
         get_grouped_ccc(data, "annotator", "gs_induration", "annot_induration", "induration"),
         get_grouped_ccc(data, "annotator", "gs_pga", "annot_pga", "pga")) %>%
        purrr::reduce(rbind) %>% 
        dplyr::arrange(annotator, metric) %>%
        dplyr::select(annotator, metric, ccc_score = .estimate)
}

replace_to_na <- function(data){
    metric <- (data %>% select(matches("^gs|^annot"), -annotator) %>% names(.))
    data %>% 
        dplyr::mutate(across(
            all_of(metric), 
            ~ as.numeric(base::replace(., . == "Can't Tell", NA))))  %>% 
        tibble::as_tibble()
}


main <- function(){
    #' get all data
    scores <- get_merged_annotation_data() %>%
        replace_to_na() %>%
        run_ccc_scoring() %>%
        readr::write_tsv(OUTPUT_FILENAME)
    
    #' save to synapse
    file <- synapser::File(OUTPUT_FILENAME, parentId = CONCORDANCE_PARENT_ID)
    synapser::synStore(file, 
                       activityName = "get_ccc",
                       used = MERGED_PARENT_ID,
                       executed = GIT_URL)
    unlink(OUTPUT_FILENAME)
}

main()
