########################################################################
# Psoriasis Validation Analysis
# Purpose: To prepare labelled data table for swollen finger joints, dactylitic fingers
# Authors: Meghasyam Tummalacherla, Aryton Tediarjos
# Maintainers: meghasyam@sagebase.org, aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

## Photo before record

##############
# Required libraries
##############
library(synapser)
library(githubr)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
source('utils/processing_log_utils.R')
source('utils/feature_extraction_utils.R')

synapser::synLogin()

##############################
# Git Reference
##############################
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
SCRIPT_PATH <- file.path('analysis', 
                         "handImaging_analysis", 
                         "curateSwollenJointsDactyliticFingers.R")
setGithubToken(
    readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = SCRIPT_PATH)

#' get visit reference and curated ppacman table
PPACMAN_TBL_ID <- "syn22337133"
VISIT_REF_ID <- "syn25825626"
visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread() %>%
    dplyr::select(participantId,
                  createdOn,
                  visit_num, 
                  diagnosis)

##############
# Download data from Synapse
##############
# Images
images.folder.id <- 'syn25837496'
images.ids.syn <- synapser::synGetChildren(images.folder.id)
images.ids <- images.ids.syn$asList() %>% 
    data.table::rbindlist()
all.used.ids <- images.folder.id

# Images original table
images.orig.tbl.id <- 'syn22281749'
images.orig.tbl.syn <- synapser::synTableQuery(paste0('SELECT * FROM ', images.orig.tbl.id))
images.orig.tbl <- images.orig.tbl.syn$asDataFrame()
all.used.ids <- c(all.used.ids, images.orig.tbl.id)

# Swollen joints
swollen.syn.id <- 'syn25832774'
swollen.tbl <- read.csv(synapser::synGet(swollen.syn.id)$path, sep = '\t')
all.used.ids <- c(all.used.ids, swollen.syn.id)

# Dactylitic fingers
dactylitis.id <- 'syn25006883'
dactylitis.syn <- synapser::synTableQuery(paste0('select * from ', dactylitis.id))
dactylitis.tbl <- dactylitis.syn$asDataFrame()
all.used.ids <- c(all.used.ids, dactylitis.id)

##############
# Data Modification
##############
## Update annotations for each image from Synapse
images.anno <- apply(images.ids,1,function(x){
    temp.anno <- synapser::synGetAnnotations(x[['id']]) %>% 
        unlist() %>% 
        as.data.frame() %>% 
        t() %>% 
        as.data.frame() %>% 
        dplyr::mutate(id =  x[['id']]) 
    
    return(temp.anno)
    
}) %>% data.table::rbindlist(fill = T) %>% 
    dplyr::select(-createdOn)

## merge image information 
## and remove old images (they do not have annotations)
images.tbl.meta <- images.ids %>% 
    dplyr::left_join(images.anno) %>% 
    dplyr::filter(!(is.na(recordId)))

## Subset images.orig.tbl to include only records for which we have curated images
images.orig.tbl <- images.orig.tbl %>% 
    dplyr::filter(recordId %in% images.tbl.meta$recordId) %>% 
    dplyr::select(recordId,
                  createdOn,
                  participantId)

### Swollen Joints
swollen.tbl.meta <- swollen.tbl %>% 
    dplyr::select(participantId, createdOn, recordId, gs_swell_joint_list) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(left_cmc_1 = as.numeric(grepl('left_cmc_1', gs_swell_joint_list)),
                  left_mcp_1 = as.numeric(grepl('left_mcp_1', gs_swell_joint_list)),
                  left_mcp_2 = as.numeric(grepl('left_mcp_2', gs_swell_joint_list)),
                  left_mcp_3 = as.numeric(grepl('left_mcp_3', gs_swell_joint_list)),
                  left_mcp_4 = as.numeric(grepl('left_mcp_4', gs_swell_joint_list)),
                  left_mcp_5 = as.numeric(grepl('left_mcp_5', gs_swell_joint_list)),
                  left_pip_1 = as.numeric(grepl('left_pip_1', gs_swell_joint_list)),
                  left_pip_2 = as.numeric(grepl('left_pip_2', gs_swell_joint_list)),
                  left_pip_3 = as.numeric(grepl('left_pip_3', gs_swell_joint_list)),
                  left_pip_4 = as.numeric(grepl('left_pip_4', gs_swell_joint_list)),
                  left_pip_5 = as.numeric(grepl('left_pip_5', gs_swell_joint_list)),
                  left_dip_2 = as.numeric(grepl('left_dip_2', gs_swell_joint_list)),
                  left_dip_3 = as.numeric(grepl('left_dip_3', gs_swell_joint_list)),
                  left_dip_4 = as.numeric(grepl('left_dip_4', gs_swell_joint_list)),
                  left_dip_5 = as.numeric(grepl('left_dip_5', gs_swell_joint_list)),
                  right_cmc_1 = as.numeric(grepl('right_cmc_1', gs_swell_joint_list)),
                  right_mcp_1 = as.numeric(grepl('right_mcp_1', gs_swell_joint_list)),
                  right_mcp_2 = as.numeric(grepl('right_mcp_2', gs_swell_joint_list)),
                  right_mcp_3 = as.numeric(grepl('right_mcp_3', gs_swell_joint_list)),
                  right_mcp_4 = as.numeric(grepl('right_mcp_4', gs_swell_joint_list)),
                  right_mcp_5 = as.numeric(grepl('right_mcp_5', gs_swell_joint_list)),
                  right_pip_1 = as.numeric(grepl('right_pip_1', gs_swell_joint_list)),
                  right_pip_2 = as.numeric(grepl('right_pip_2', gs_swell_joint_list)),
                  right_pip_3 = as.numeric(grepl('right_pip_3', gs_swell_joint_list)),
                  right_pip_4 = as.numeric(grepl('right_pip_4', gs_swell_joint_list)),
                  right_pip_5 = as.numeric(grepl('right_pip_5', gs_swell_joint_list)),
                  right_dip_2 = as.numeric(grepl('right_dip_2', gs_swell_joint_list)),
                  right_dip_3 = as.numeric(grepl('right_dip_3', gs_swell_joint_list)),
                  right_dip_4 = as.numeric(grepl('right_dip_4', gs_swell_joint_list)),
                  right_dip_5 = as.numeric(grepl('right_dip_5', gs_swell_joint_list))) %>% 
    dplyr::ungroup()

### Dactylitic fingers
dactylitis.tbl.meta <- dactylitis.tbl %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(finger_dactylitis = ifelse(grepl('L|R', `Finger Dactylitis`), 1,0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(participantId, visit_num = `Visit Number`, finger_dactylitis) %>% 
    unique()

### Merge Images, Swollen Joints, Dactylitic Fingers joints
images_data <- images.tbl.meta %>% 
    dplyr::mutate(createdOn = lubridate::as_datetime(createdOn)) %>%
    dplyr::select(name,
                  synId = id,
                  recordId,
                  participantId) %>% 
    dplyr::left_join(images.orig.tbl) %>% 
    join_with_ppacman(visit_ref, ppacman) %>%
    dplyr::left_join(dactylitis.tbl.meta)

joint_swell_data <- swollen.tbl.meta %>% 
    dplyr::mutate(createdOn = lubridate::as_datetime(createdOn)) %>%
    dplyr::select(-recordId, -gs_swell_joint_list) %>% 
    join_with_ppacman(visit_ref, ppacman)

merged.tbl <- images_data %>%
    dplyr::left_join(joint_swell_data)

## Upload new table to Synapse
synapse.folder.id <- 'syn22342373' # synId of folder to upload your file to
OUTPUT_FILE <- 'HandImages_SwollenJoints_DactyliticFingers.tsv' # name your file
write.table(merged.tbl, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         used = all.used.ids,
         executed = GIT_URL)
unlink(OUTPUT_FILE)