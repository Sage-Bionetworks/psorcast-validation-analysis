#########################
#' Function to map 
#' based on Elias modelling data
#'
#########################
library(synapser)
library(ggplot2)
library(tidyverse)
library(ROCit)

#' login to Synapse
synapser::synLogin()

#' get Elias Synapse ID
REF_LIST <- list(
    djo = list(
        combined_upper_pain = "syn26163179",
        upper_joint_pain = "syn26163180",
        output = "DjO_model_roc_auc_data.tsv",
        output_iter = "DjO_model_boxplot_data.tsv"
    )
)

PARENT_ID <- "syn25704998"


get_med_auc_info <- function(data){
    #' get median AUCs across folds
    auc_data <- data %>% 
        dplyr::mutate(n_row = row_number())
    med_auc <- auc_data %>%
        dplyr::summarise(adjusted = median(adjusted),
                         original = median(original),
                         shuffled = median(shuffled))
    auc_data %>%
        dplyr::filter(near(adjusted, med_auc$adjusted, tol = 0.001)) %>%
        dplyr::slice(2) %>%
        dplyr::mutate(md_auc = sprintf(adjusted, fmt = '%#.2f')) %>%
        dplyr::select(md_auc, n_row)
}

visualize_roc_auc <- function(data){
    random_data <- data %>%
        dplyr::filter(group == "Random Chance")
    data %>%
        dplyr::filter(group != "Random Chance") %>%
        ggplot(aes(x = fpr, y = tpr, color = group)) +
        geom_line() +
        geom_line(data = random_data, 
                  aes(x = fpr, y = tpr, color = group),
                  linetype = "dashed") +
        geom_point()
}

visualize_group_boxplot <- function(data){
    data %>%
        ggplot(aes(x = subgroup, 
                   y = auc_score, 
                   fill = subgroup)) +
        geom_boxplot(alpha = 0.6) +
        geom_hline(yintercept = 0.5, 
                   linetype = "twodash", 
                   color = "red",
                   show.legend = TRUE) +
        theme_minimal() +
        theme(axis.text.x = element_blank())
}


#' get combined upper pain
load(synapser::synGet(
    REF_LIST$djo$combined_upper_pain)$path)
combined_upper_pain_adjusted_outputs <- adjusted.outputs
combined_upper_pain_aucs <- AUCs %>%
    tibble::as_tibble() %>%
    dplyr::mutate(group = "Upper Joint Pain + Enthesitis")

#' get upper joint pain
load(synapser::synGet(
    REF_LIST$djo$upper_joint_pain)$path)
upper_pain_adjusted_outputs <- adjusted.outputs
upper_pain_aucs <- AUCs %>%
    tibble::as_tibble() %>%
    dplyr::mutate(group = "Upper Joint Pain")

    
# get boxplot
auc_iter <- combined_upper_pain_aucs %>%
    dplyr::bind_rows(upper_pain_aucs) %>%
    tidyr::pivot_longer(cols = c("adjusted", "shuffled", "original"),
                        values_to = "auc_score") %>%
    dplyr::mutate(subgroup = paste(group,name))

auc_iter_plot <- auc_iter %>%
    visualize_group_boxplot() +
    labs(title = "AUC scores on 1k Random-Forest Iterations",
         subtitle = "Comparison AUC scores on different subgroups")

row_loc <- list(
    combined = combined_upper_pain_aucs %>% 
        get_med_auc_info() %>%
        dplyr::mutate(auc_string = glue::glue("Upper Joint Pain + Enthesitis\n(AUC: {md_auc})\n")),
    upper_pain = upper_pain_aucs %>% 
        get_med_auc_info() %>%
        dplyr::mutate(auc_string = glue::glue("Upper Joint Pain\n(AUC: {md_auc})\n"))
)



combined_score <- 
    rocit(
        score=combined_upper_pain_adjusted_outputs[[row_loc$combined$n_row]]$test.data$pred.prob,
        class=combined_upper_pain_adjusted_outputs[[row_loc$combined$n_row]]$test.data$combined_upper_pain)
combined_data <- tibble::tibble(
    fpr = combined_score$FPR, 
    tpr = combined_score$TPR) %>%
    dplyr::mutate(group = "Upper Joint Pain + Enthesitis")

upper_pain_score <- 
    rocit(
        score=upper_pain_adjusted_outputs[[row_loc$upper_pain$n_row]]$test.data$pred.prob,
        class=upper_pain_adjusted_outputs[[row_loc$upper_pain$n_row]]$test.data$upper_body_pain)
upper_pain_data <- tibble::tibble(
    fpr = upper_pain_score$FPR, 
    tpr = upper_pain_score$TPR) %>%
    dplyr::mutate(group = "Upper Joint Pain")

random_data <- tibble::tibble(
    fpr = seq(0, 1, length.out = 30),
    tpr = seq(0, 1, length.out = 30)) %>%
    dplyr::mutate(group = "Random Chance")

result <- dplyr::bind_rows(
    upper_pain_data, 
    combined_data,
    random_data)

roc_auc_plots <- result %>%
    visualize_roc_auc() +
    scale_color_manual(
        name="Group",
        labels = c(
            "Random",
            row_loc$upper_pain$auc_string,
            row_loc$combined$auc_string),
        values=c(
            "red1", 
            "orange2",
            "skyblue3")) +
    theme_minimal() +
    labs(
        title= "Digital Jar Opener ROC-AUC Curves",
        subtitle = "Modeling based on upper-pain outcome variables")

plot <- wrap_plots(auc_iter_plot, roc_auc_plots, nrow = 2, ncol = 1)
    
result %>%
    readr::write_tsv(REF_LIST$djo$output)
file = synapser::File(REF_LIST$djo$output, 
                      parent = PARENT_ID)
activity = synapser::Activity(
    used = c(REF_LIST$djo$combined_upper_pain,
             REF_LIST$djo$upper_joint_pain))
synStore(file, activity = activity)

auc_iter %>%
    readr::write_tsv(REF_LIST$djo$output_iter)
file = synapser::File(REF_LIST$djo$output_iter, 
                      parent = PARENT_ID)
activity = synapser::Activity(
    used = c(REF_LIST$djo$combined_upper_pain,
             REF_LIST$djo$upper_joint_pain))
synStore(file, activity = activity) 

