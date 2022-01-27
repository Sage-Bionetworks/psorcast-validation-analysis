library(synapser)
library(data.table)
library(tidyverse)

synapser::synLogin()


get_feature_extraction_id <- function(data, 
                                      analysis_type,
                                      task_type = NULL){
    subset <- data %>% 
        dplyr::filter(
            analysisType == analysis_type,
            pipelineStep == "feature extraction")
    if(!is.null(task_type)){
        subset <- subset %>% 
            dplyr::filter(task == task_type)
    }
    return(subset)
}

file_view_id <- synapser::synFindEntityId(
    "Psorcast Manuscript - File View", "syn26840742")
tbl_df <- synTableQuery(
    glue::glue("SELECT * FROM {file_view_id}"))$asDataFrame()

SYN_ID_REF <- list()
SYN_ID_REF$removed_data <- list(
    parent_id = tbl_df %>% 
        dplyr::filter(type == "folder", 
                      name== "Removed Data Log") %>% 
        .$id
)
SYN_ID_REF$feature_extraction <- list(
    parent_id = tbl_df %>% 
        dplyr::filter(type == "folder", 
                      name== "Features") %>% 
        .$id,
    ppacman = get_feature_extraction_id(
        tbl_df, 
        analysis_type = "clinical data") %>% .$id,
    visit_summary =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "visit summary") %>% .$id,
    djo =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "digital jar open") %>% .$id,
    draw =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "psoriasis draw") %>% .$id,
    dig_jc =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "joint counts analysis",
        task_type = "digital joint count") %>% .$id,
    gs_jc =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "joint counts analysis",
        task_type = "gold-standard joint count") %>% .$id,
    gs_js =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "joint counts analysis",
        task_type = "gold-standard joint swell") %>% .$id,
    merged =  get_feature_extraction_id(
        tbl_df, 
        analysis_type = "merged feature files") %>% .$id
)

SYN_ID_REF$curated_features <- list(
    parent = tbl_df %>% 
        dplyr::filter(type == "folder",
                      name == "Curated Features") %>% .$id,
    curated_djo = tbl_df %>% 
        dplyr::filter(analysisType == "digital jar open",
                      pipelineStep == "feature curation") %>% .$id
)


SYN_ID_REF$model_performance <- list(
    parent = tbl_df %>% 
        dplyr::filter(type == "folder",
                      name == "Model Performance") %>% .$id,
    psa_pso_md_fpr_tpr = tbl_df %>% 
        dplyr::filter(analysisType == "digital jar open",
                      analysisSubtype == "psa vs pso - median iter",
                      pipelineStep == "prediction") %>% .$id,
    psa_pso_auc_iter = tbl_df %>% 
        dplyr::filter(analysisType == "digital jar open",
                      analysisSubtype == "psa vs pso - auc iter",
                      pipelineStep == "prediction") %>% .$id,
    uei_pso_md_fpr_tpr = tbl_df %>% 
        dplyr::filter(analysisType == "digital jar open",
                      analysisSubtype == "uei - median iter",
                      pipelineStep == "prediction") %>% .$id,
    uei_pso_auc_iter = tbl_df %>% 
        dplyr::filter(analysisType == "digital jar open",
                      analysisSubtype == "uei - auc iter",
                      pipelineStep == "prediction") %>% .$id
)

SYN_ID_REF$figures <- list(
    parent = tbl_df %>% 
        dplyr::filter(type == "folder",
                      name == "Figures") %>% .$id
)
