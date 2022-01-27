########################################################################
# Psoriasis Validation
# Purpose: To extract features for the Digital Jar Open test
# Maintainer : Meghasyam Tummalacherla, Aryton Tediarjo
# email: meghasyam@sagebase.org, aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(githubr)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
source("manuscript/utils/feature_extraction_utils.R")
source('manuscript/utils/helper_utils.R')
source("manuscript/utils/fetch_id_utils.R")

synapser::synLogin()

# source table ids
DIG_JAR_OPEN_TBL_ID <- config::get("tables")$jar_opener
PPACMAN_TBL_ID <- SYN_ID_REF$feature_extraction$ppacman
VISIT_REF_ID <- SYN_ID_REF$feature_extraction$visit_summary
OUTPUT_REF <- list(
    feature = list(
        output = 'djo_rotation_features.tsv',
        parent = SYN_ID_REF$feature_extraction$parent_id),
    log = list(
        output = 'error_log_djo_rotation_features.tsv',
        parent = SYN_ID_REF$removed_data$parent_id)
)

# Github link
SCRIPT_PATH <- file.path(
    'manuscript',
    'feature_extraction', 
    "digitalJarOpen_rotation_features.R")
GIT_URL <- get_github_url(
    git_token_path = config::get("git")$token_path,
    git_repo = config::get("git")$repo,
    script_path = SCRIPT_PATH,
    ref="branch", 
    refName='main')

main <- function(){
    #' get visit reference and curated ppacman table
    visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
    ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread()
    
    #' get djo features from table
    digitalJarOpen.syn <- synapser::synTableQuery(paste0(
        'select * from ', DIG_JAR_OPEN_TBL_ID))
    digitalJarOpen.tbl <- digitalJarOpen.syn$asDataFrame() %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(djo_inward_ratio = 
                          max(leftClockwiseRotation, 
                              rightCounterRotation)/min(leftClockwiseRotation, 
                                                        rightCounterRotation),
                      djo_outward_ratio = 
                          max(rightClockwiseRotation, 
                              leftCounterRotation)/min(rightClockwiseRotation, 
                                                       leftCounterRotation)) %>% 
        dplyr::ungroup()
    
    #' get all djo features
    all.digitalJarOpen.ftrs <- digitalJarOpen.tbl %>% 
        dplyr::select(-ROW_ID, -ROW_VERSION) %>% 
        dplyr::mutate(createdOn = as.character(createdOn))
    
    #' get data joinnable with ppacman
    digitalJarOpen.ftrs <- all.digitalJarOpen.ftrs %>%
        join_with_ppacman(visit_ref, ppacman) %>% 
        dplyr::mutate(createdOn = as.character(createdOn)) %>%
        dplyr::select(recordId, 
                      participantId, 
                      createdOn, 
                      visit_num, 
                      starts_with("djo"),
                      djo_leftClockwise = leftClockwiseRotation,
                      djo_leftCounter = leftCounterRotation,
                      djo_rightClockwise = rightClockwiseRotation,
                      djo_rightCounter = rightCounterRotation) %>% 
        dplyr::group_by(participantId, visit_num) %>%
        dplyr::summarise_all(last) %>%
        dplyr::arrange(desc(createdOn))
    
    #' get error logging for removed records
    error_log <- log_removed_data(all.digitalJarOpen.ftrs %>%
                                      dplyr::mutate(error = NA), 
                                  digitalJarOpen.ftrs)
    
    entity <- save_to_synapse(
        data = digitalJarOpen.ftrs,
        output_filename = OUTPUT_REF$feature$output,
        parent = OUTPUT_REF$feature$parent,
        name = "get dig jar opener feat",
        executed = GIT_URL,
        used = DIG_JAR_OPEN_TBL_ID)
    synSetAnnotations(
        entity$properties$id,
        analysisType = "digital jar open",
        pipelineStep = "feature extraction",
        task = "digital jar open")
    
    entity <- save_to_synapse(
        data = error_log,
        output_filename = OUTPUT_REF$log$output,
        parent = OUTPUT_REF$log$parent,
        name = "get dig jar opener feat - log",
        executed = GIT_URL,
        used = DIG_JAR_OPEN_TBL_ID)
    synSetAnnotations(
        entity$properties$id,
        analysisType = "digital jar open",
        pipelineStep = "removed data log",
        task = "digital jar open")
}

log_process(main(), SCRIPT_PATH)
