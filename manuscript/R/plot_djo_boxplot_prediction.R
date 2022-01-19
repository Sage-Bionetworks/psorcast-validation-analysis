#########################
#' Function to map 
#' based on Elias modelling data
#########################
library(synapser)
library(ggplot2)
library(tidyverse)
library(ROCit)
synapser::synLogin()

PARENT_ID <- "syn26840743"
INPUT_REF <- list(
    djo_curated = "syn26842131",
    upper_pain_auc_iter = "syn26842243",
    upper_pain_fpr_tpr = "syn26842242",
    psa_vs_pso_auc_iter = "syn26842137",
    psa_vs_pso_fpr_tpr = "syn26842241"
)

OUTPUT_REF <- list(
    overall_rotation_diagnosis = list(plot = NULL,
                                      output_file = "Figure_3D.png"),
    auc_roc_curve =  list(plot = NULL,
                          output_file = "Figure_3F.png"),
    shuffled_vs_adjusted =  list(plot = NULL,
                                 output_file = "Figure_3E.png")
)

djo_curated <- synGet(INPUT_REF$djo_curated)$path %>% 
    fread() %>% 
    dplyr::mutate(diagnosis = factor(
        diagnosis, 
        levels = c("Control", "PsO", "PsA")))

uei <- synapser::synGet(INPUT_REF$upper_pain_auc_iter)$path %>% 
    fread() %>% 
    dplyr::select(-original) %>%
    tidyr::pivot_longer(cols = c("shuffled", "adjusted")) %>% 
    dplyr::mutate(label = ifelse(name == "adjusted", 
                                 "Upper Extremity Involvement",
                                 "Shuffled-Label (UEI)"))
uei_fpr_tpr <- synapser::synGet(
    INPUT_REF$upper_pain_fpr_tpr)$path %>% 
    fread()

psa_vs_pso <- synapser::synGet(INPUT_REF$psa_vs_pso_auc_iter)$path %>% 
    fread() %>% 
    dplyr::select(-original) %>%
    tidyr::pivot_longer(cols = c("shuffled", "adjusted")) %>% 
    dplyr::mutate(label = ifelse(name == "adjusted", 
                                 "PsA Diagnosis",
                                 "Shuffled-Label (PsO vs PsA)"))
psa_vs_pso_fpr_tpr <- synapser::synGet(
    INPUT_REF$psa_vs_pso_fpr_tpr)$path %>% 
    fread()

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
    geom_boxplot(width = 0.5) +
    geom_point(position = position_jitternormal(),
               alpha = 0.05) +
    scale_color_manual(values = c("grey", "blue", "grey", "red")) +
    labs(title = "Digital Jar Opener Classification Model",
         y = "ROC AUC") +
    theme_minimal() +
    theme(legend.position = "none") 

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
    theme_minimal() +
    labs(y = "True Positive Rate", 
         x = "False Positive Rate",
         title = "Digital Jar Open\nMedian Model ROC Curves") +
    theme(legend.title = element_blank())


OUTPUT_REF$overall_rotation_diagnosis$plot <- djo_curated  %>%    
    ggplot(aes(x = diagnosis, y = total_rotation, color = diagnosis)) +
    geom_boxplot(outlier.shape = NA) +
    ggpubr::stat_compare_means(
        label = "p.signif",
        comparisons = list(c("PsO", "PsA"), c("Control", "PsA"))) +
    geom_point(position = position_jitternormal(),
               alpha = 0.1) +
    scale_color_manual(values = c("grey", "grey", "red")) +
    labs(
        title = "Digital Jar Open\nOverall Rotation",
        y = "Overall Rotation") +
    theme_minimal()


purrr::map(OUTPUT_REF, function(content){
    ggsave(content$output_file, content$plot, width = 8)
    file = synapser::File(content$output_file, parent = PARENT_ID)
    synStore(file)
    unlink(content$output_file)
})


