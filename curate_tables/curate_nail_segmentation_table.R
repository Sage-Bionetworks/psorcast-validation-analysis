####################################################
#' Script for filtering nail annotation 
#' dataframe with Dan's Annotation
#' to save annotating time for collaborator
#' 
#' maintainer: aryton.tediarjo@sagebase.org
####################################################
library(synapser)
library(data.table)
library(dplyr)
library(githubr)

synapser::synLogin()

#' Global Variable
HAND_IMAGING_TBL_ID <- "syn26050060"
ANNOTATION <- "syn26148075"
TABLE_NAME <- "HandImaging - FingerSegmentation (Curated)"
PARENT_ID <- "syn22276946"

#' get git reference
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- file.path('curate_tables',
                         'curate_nail_segmentation_table.R')
setGithubToken(
    readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='model'), 
    repositoryPath = SCRIPT_PATH)

#' function to create nail annotation table column object
create_columns <- function(){
    list(
        Column(name = "recordId", columnType = "STRING", maximumSize = 50),
        Column(name = "createdOn", columnType = "DATE"),
        Column(name = "participantId", columnType = "STRING", maximumSize = 50),
        Column(name = "finger_key", columnType = "STRING", maximumSize = 50),
        Column(name = "finger_segments", columnType = "FILEHANDLEID"))
}



#' get reference annotation data
annotation_data <- synGet(ANNOTATION)$path %>% 
    fread() %>%
    dplyr::filter(nail_pso_status != "Can't Tell") %>%
    dplyr::select(recordId, finger_key)

#' filter all nail annotation data using reference annotation data
table <- synTableQuery(
    glue::glue("SELECT * FROM {HAND_IMAGING_TBL_ID}"))$asDataFrame() %>%
    dplyr::inner_join(annotation_data, by = c("recordId", "finger_key")) %>%
    dplyr::select(-ROW_ID, -ROW_VERSION)

#' find if old id exist, if it exists
#' remove rows so that it can be refreshed with
#' new data
id <- synFindEntityId(TABLE_NAME, parent = PARENT_ID)
if(!is.null(id)){
    row <- synTableQuery(glue::glue("SELECT * FROM {id}"))
    synDelete(row)
}

#' create schema, and save dataframe
schema <- Schema(name = TABLE_NAME,
                 columns = create_columns(),
                 parent = PARENT_ID)
table <- Table(schema, table)
activity <- Activity(used = HAND_IMAGING_TBL_ID,
                     executed = GIT_URL)
table <- synStore(table)

