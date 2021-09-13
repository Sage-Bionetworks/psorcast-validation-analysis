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
COMBINED_UPPER_PAIN <- "syn26163179"
UPPER_JOINT_PAIN <- "syn26163180"


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


#' get combined upper pain
load(synapser::synGet(COMBINED_UPPER_PAIN)$path)
combined_upper_pain_adjusted_outputs <- adjusted.outputs
combined_upper_pain_aucs <- AUCs %>%
    tibble::as_tibble()

#' get upper joint pain
load(synapser::synGet(UPPER_JOINT_PAIN)$path)
upper_pain_adjusted_outputs <- adjusted.outputs
upper_pain_aucs <- AUCs %>%
    tibble::as_tibble()


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

auc_plots <- result %>%
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
    
