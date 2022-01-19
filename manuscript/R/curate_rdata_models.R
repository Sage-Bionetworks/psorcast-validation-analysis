#########################
#' Function to map 
#' based on Elias modelling data
#########################
library(synapser)
library(ggplot2)
library(tidyverse)
library(ROCit)

#' login to Synapse
synapser::synLogin()

#' get Elias Synapse ID
REF_LIST <- list(
    psa_vs_pso = list(
        id = "syn26523864",
        output_tpr_fpr = "djo_model_psa_vs_pso_tpr_fpr.tsv",
        output_iter = "djo_model_psa_vs_pso_auc_iter.tsv",
        output_plot = "djo_model_psa_vs_pso_plot.png",
        rdata_key = "PsA",
        glue_label = "PsA vs PsO ({auc})"
    ),
    combined_upper_pain = list(
        id = "syn26523863",
        output_tpr_fpr = "djo_model_combined_upper_pain_tpr_fpr.tsv",
        output_iter = "djo_model_combined_upper_pain_auc_iter.tsv",
        output_plot = "djo_model_combined_upper_pain_plot.png",
        rdata_key = "combined_upper_pain",
        glue_label = "Combined Upper Pain ({auc})"
        
    )
)

PARENT_ID <- "syn26842135"


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

fetch_rdata <- function(id){
    #' get combined upper pain
    load(synapser::synGet(id)$path)
    metrics_list <- list(
        adjusted_outputs = adjusted.outputs,
        auc_iteration = AUROCs %>%
            tibble::as_tibble()
    )
    return(metrics_list)
}

plot_auc_iter <- function(auc_iter){
    auc_iter %>% 
        tidyr::pivot_longer(cols = c("adjusted", "shuffled", "original"),
                            values_to = "auc_score") %>%
        ggplot(aes(x = name, 
                   y = auc_score, 
                   fill = name)) +
        geom_boxplot(alpha = 0.6) +
        geom_hline(yintercept = 0.5, 
                   linetype = "twodash", 
                   color = "red",
                   show.legend = TRUE) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              legend.position = "none")
}


purrr::map(names(REF_LIST), function(ref){
    rdata_key = REF_LIST[[ref]]$rdata_key
    glue_label = REF_LIST[[ref]]$glue_label
    metrics_list <- fetch_rdata(REF_LIST[[ref]]$id)
    row_loc <- list(
        combined = metrics_list$auc_iteration %>% 
            get_med_auc_info() %>%
            dplyr::mutate(auc_string = glue::glue(
                "AUC: {md_auc}"))
    )
    
    
    tpr_fpr <- 
        rocit(
            score=metrics_list$adjusted_outputs[[row_loc$combined$n_row]]$test.data$pred.prob,
            class=metrics_list$adjusted_outputs[[row_loc$combined$n_row]]$test.data[[rdata_key]])
    tpr_fpr <- tibble::tibble(
        fpr = tpr_fpr$FPR, 
        tpr = tpr_fpr$TPR) %>%
        dplyr::mutate(group = glue::glue(
            glue_label, 
            auc = row_loc$combined$auc_string[[1]]))
    random_data <- tibble::tibble(
        fpr = seq(0, 1, length.out = 30),
        tpr = seq(0, 1, length.out = 30)) %>%
        dplyr::mutate(group = "Random Chance")
    
    result <- dplyr::bind_rows(
        tpr_fpr,
        random_data)
    
    result %>%
        readr::write_tsv(REF_LIST[[ref]]$output_tpr_fpr)
    metrics_list$auc_iteration %>% 
        readr::write_tsv(REF_LIST[[ref]]$output_iter)
    

    file = synapser::File(REF_LIST[[ref]]$output_tpr_fpr, 
                          parent = PARENT_ID)
    activity = synapser::Activity(
        used = c(REF_LIST[[ref]]$id))
    synStore(file, activity = activity)
    
    
    file = synapser::File(REF_LIST[[ref]]$output_iter, 
                          parent = PARENT_ID)
    activity = synapser::Activity(
        used = c(REF_LIST[[ref]]$id))
    synStore(file, activity = activity) 
    
    
    file = synapser::File(REF_LIST[[ref]]$output_tpr_fpr, 
                          parent = PARENT_ID)
    activity = synapser::Activity(
        used = c(REF_LIST[[ref]]$id))
    synStore(file, activity = activity)
    
})
