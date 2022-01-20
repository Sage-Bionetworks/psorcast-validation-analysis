##################################################
#' Function to plot boxplot
#' of prediction model of 
#' digital jar open digital task
#' 
#' @author: aryton.tediarjo@sagebase.org
##################################################
library(synapser)
library(ggplot2)
library(tidyverse)
library(ROCit)
library(ggforce)
source("manuscript/utils/fetch_id_utils.R")
source("manuscript/utils/helper_utils.R")
synapser::synLogin()

# get github url
GIT_URL <- get_github_url(
    git_token_path = config::get("git")$token_path,
    git_repo = config::get("git")$repo,
    script_path = "manuscript/figures/plo_djo_boxplot_prediction.R",
    ref="branch", 
    refName='main')

# get parent id reference
PARENT_ID <- SYN_ID_REF$figures$parent

# input reference
INPUT_REF <- list(
    djo_curated = SYN_ID_REF$curated_features$curated_djo,
    upper_pain_auc_iter = SYN_ID_REF$model_performance$uei_pso_auc_iter,
    upper_pain_fpr_tpr = SYN_ID_REF$model_performance$uei_pso_md_fpr_tpr,
    psa_vs_pso_auc_iter = SYN_ID_REF$model_performance$psa_pso_auc_iter,
    psa_vs_pso_fpr_tpr = SYN_ID_REF$model_performance$psa_pso_md_fpr_tpr
)

# output reference
OUTPUT_REF <- list(
    overall_rotation_diagnosis = list(
        plot = NULL,
        used = c(INPUT_REF$djo_curated),
        output_file = "Figure_3D.png",
        analysisType = "digital jar open",
        analysisSubtype = "overall rotation comparison",
        pipelineStep = "figures",
        name = "generate plot for overall rotation",
        description = "compare overall rotation across groups"),
    auc_roc_curve =  list(
        plot = NULL,
        used = c(INPUT_REF$upper_pain_fpr_tpr,
                 INPUT_REF$psa_vs_pso_fpr_tpr),
        output_file = "Figure_3F.png",
        analysisType = "digital jar open",
        analysisSubtype = "Prediction ROC-AUC Curve",
        pipelineStep = "figures",
        name = "plot ROC-AUC curve",
        description = "compare ROC-AUC Curves based on different clinical endpoint"),
    shuffled_vs_adjusted =  list(
        plot = NULL,
        used = c(INPUT_REF$upper_pain_auc_iter,
                 INPUT_REF$psa_vs_pso_auc_iter),
        output_file = "Figure_3E.png",
        analysisType = "digital jar open",
        analysisSubtype = "overall rotation comparison",
        pipelineStep = "figures",
        name = "generate AUC iteration (1000 fold) scores acrosss groups",
        description = "compare AUC iteration (1000 fold) across groups")
)

# read djo curated features
djo_curated <- synGet(INPUT_REF$djo_curated)$path %>% 
    fread() %>% 
    dplyr::mutate(diagnosis = factor(
        diagnosis, 
        levels = c("Control", "PsO", "PsA")))

# get UEI auc across 1kfold
uei <- synapser::synGet(
    INPUT_REF$upper_pain_auc_iter)$path %>% 
    fread() %>% 
    dplyr::select(-original) %>%
    tidyr::pivot_longer(cols = c(
        "shuffled", "adjusted")) %>% 
    dplyr::mutate(label = ifelse(
        name == "adjusted", 
        "Upper Extremity Involvement",
        "Shuffled-Label (UEI)"))

# get uei TPR and FPR based on median iter
uei_fpr_tpr <- synapser::synGet(
    INPUT_REF$upper_pain_fpr_tpr)$path %>% 
    fread()

# get psa vs pso auc across 1kfold
psa_vs_pso <- synapser::synGet(INPUT_REF$psa_vs_pso_auc_iter)$path %>% 
    fread() %>% 
    dplyr::select(-original) %>%
    tidyr::pivot_longer(cols = c("shuffled", 
                                 "adjusted")) %>% 
    dplyr::mutate(
        label = ifelse(
            name == "adjusted", 
            "PsA Diagnosis",
            "Shuffled-Label (PsO vs PsA)"))

# get psa vs pso TPR/FPR of median AUC
psa_vs_pso_fpr_tpr <- synapser::synGet(
    INPUT_REF$psa_vs_pso_fpr_tpr)$path %>% 
    fread()

# plot boxplot for shuffled vs adjusted
OUTPUT_REF$shuffled_vs_adjusted$plot <- dplyr::bind_rows(psa_vs_pso, uei) %>%
    dplyr::mutate(label = factor(
        label, 
        levels = c("Shuffled-Label (UEI)",
                   "Upper Extremity Involvement",
                   "Shuffled-Label (PsO vs PsA)",
                   "PsA Diagnosis"))) %>%
    ggplot(aes(x = label, 
               y = value, 
               color = label)) +
    geom_boxplot(width = 0.35) +
    geom_point(position = position_jitternormal(),
               alpha = 0.05) +
    scale_color_manual(
        values = c("grey", "blue", "grey", "red")) +
    labs(title = "Digital Jar Opener Classification Model",
         y = "ROC AUC",
         x = "") +
    theme_classic() +
    theme(legend.position = "none") 

# plot ROC-AUC curves
OUTPUT_REF$auc_roc_curve$plot <- dplyr::bind_rows(psa_vs_pso_fpr_tpr, 
                                 uei_fpr_tpr) %>%
    dplyr::mutate(group = factor(
        group,
        levels = c("Random Chance",
                   "Combined Upper Pain (AUC: 0.68)",
                   "PsA vs PsO (AUC: 0.73)"))) %>% 
    ggplot(aes(x = fpr, y = tpr, color = group)) +
    geom_line() +
    scale_color_manual(values = c("black", "blue", "red")) +
    theme_classic() +
    labs(y = "True Positive Rate", 
         x = "False Positive Rate",
         title = "Digital Jar Open\nMedian Model ROC Curves") +
    theme(legend.title = element_blank())

# plot overall rotation comparison across different groups
OUTPUT_REF$overall_rotation_diagnosis$plot <- djo_curated  %>%    
    ggplot(aes(x = diagnosis, 
               y = total_rotation, 
               color = diagnosis)) +
    geom_boxplot(outlier.shape = NA,
                 width = 0.35) +
    ggpubr::stat_compare_means(
        label = "p.signif",
        comparisons = list(c("PsO", "PsA"), 
                           c("Control", "PsA"))) +
    geom_point(position = position_jitternormal(),
               alpha = 0.1) +
    scale_color_manual(values = c("grey", "grey", "red")) +
    labs(
        title = "Digital Jar Open\nOverall Rotation",
        y = "Overall Rotation",
        x = "") +
    theme_classic()

# iteratively save plot to synapse
purrr::map(OUTPUT_REF, function(content){
    ggsave(content$output_file, 
           content$plot, 
           width = 8)
    file <- synapser::File(
        content$output_file, 
        parent = PARENT_ID,
        analysisType = content$analysisType,
        analysisSubtype = content$analysisSubtype,
        pipelineStep = content$pipelineStep)
    activity <- Activity(
        used = content$used,
        executed = GIT_URL,
        name = content$name,
        description = content$description)
    synStore(file, activity = activity)
    unlink(content$output_file)
})


