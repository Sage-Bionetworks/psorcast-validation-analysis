################################################
# This script is used for plotting
# bland altman figures used in the manuscript
# @Author: aryton.tediarjo@sagebase.org
################################################
library(synapser)
library(knit2synapse)
library(tidyverse)
library(dplyr)
library(githubr)
library(ggpval)
library(ggExtra)
library(ggpubr)
library(data.table)
library(DescTools)
source("manuscript/utils/fetch_id_utils.R")
source("manuscript/utils/feature_extraction_utils.R")
synLogin()

# get merged features
MERGED_FEATURES <- SYN_ID_REF$feature_extraction$merged
PARENT_ID <- SYN_ID_REF$figures$parent
OUTPUT_REF <- list(
    draw = list(plot = NULL, 
                output_file = "Figure_1B.png",
                analysisType = "psoriasis draw",
                analysisSubtype = "concordance analysis",
                pipelineStep = "figures",
                task = "psoriasis draw",
                description = "Bland-Altman Plot with CCC score for figure 1B",
                name = "Plot PsoDraw Concordance"),
    tjc = list(plot = NULL, 
               output_file = "Figure_2B.png",
               analysisType = "joint counts analysis",
               analysisSubtype = "concordance analysis",
               pipelineStep = "figures",
               task = "digital joint count",
               description = "Bland-Altman Plot with CCC score for figure 2B",
               name = "Plot joint counts Concordance")
)

GIT_URL <- get_github_url(
    git_token_path = config::get("git")$token_path,
    git_repo = config::get("git")$repo,
    script_path = "manuscript/figures/plot_bland_altman_figures.R",
    ref="branch", 
    refName='main'
)

#' Function to plot bland altman
#' @param data dataframe 
#' @param dig_measure digital measurement
#' @param gs_measure gold-standard measurement
#' @param label_x_coord x-coordinate label
#' @param label_y_coord y-coordinate label
#' @return ggplot object bland-altman
plot_bland_altman <- function(data, 
                              dig_measure, 
                              gs_measure,
                              label_x_coord,
                              label_y_coord){
    # get correlation values
    cc.corr <- DescTools::CCC(
        data[[dig_measure]], 
        data[[gs_measure]], 
        ci = "z-transform", 
        conf.level = 0.95, 
        na.rm = FALSE) %>% 
        .$rho.c
    
    label <- glue::glue("CCC = {ccc} ({lci}, {uci})",
                        lci = round(cc.corr$lwr.ci, 2),
                        uci = round(cc.corr$upr.ci, 2),
                        ccc = round(cc.corr$est, 2))
    
    bland.altman <- blandr::blandr.draw(
        data[[dig_measure]], 
        data[[gs_measure]], 
        ciDisplay = FALSE,
        point_size = 3) +
        geom_point(color = "dark green") +
        annotate("text", 
                 label = label, 
                 x = label_x_coord, 
                 y = label_y_coord, 
                 size = 4)
    return(bland.altman)
}

# get dataset
data <-  fread(synGet(MERGED_FEATURES)$path)

# get digital joint count vs gold-standard joint count
OUTPUT_REF$tjc$plot <- plot_bland_altman(
    data %>%
        tidyr::drop_na(dig_jc_counts, gs_jc_counts),
    dig_measure = "dig_jc_counts",
    gs_measure = "gs_jc_counts",
    label_x_coord = 15,
    label_y_coord = 40) +
    scale_y_continuous(name = "TJC Difference", 
                       expand=c(0,0), 
                       limits=c(-50,50)) +
    labs(title = "Patient vs Physician Tender Joint Count") + 
    theme_classic() + 
    theme(plot.title = element_text(
        family = "sans", vjust = -1, 
        size = 20, margin=margin(0,0,60,0)))

# get psoriasis draw in-clinic vs digital bland-altman
OUTPUT_REF$draw$plot <- plot_bland_altman(
    data %>% 
        dplyr::mutate(dig_bsa = dig_bsa * 100) %>%
        drop_na(c(dig_bsa, gs_bsa)),
    dig_measure = "dig_bsa",
    gs_measure = "gs_bsa",
    label_x_coord = 15,
    label_y_coord = 20) +
    scale_y_continuous(name = "BSA Difference", 
                       expand=c(0,0), 
                       limits=c(-25,25)) +
    labs(title = "Patient Digital vs Physician Body Surface Area") + 
    theme_classic() + 
    theme(plot.title = element_text(
        family = "sans", vjust = -1, 
        size = 20, margin=margin(0,0,60,0)))

# iteratively save to synapse
purrr::map(OUTPUT_REF, function(content){
    ggsave(content$output_file, content$plot)
    file = synapser::File(
        content$output_file, 
        parent = PARENT_ID,
        analysisType = content$analysisType,
        analysisSubtype = content$analysisSubtype,
        pipelineStep = content$pipelineStep,
        task = content$task)
    activity = Activity(used = MERGED_FEATURES,
                        executed = GIT_URL,
                        name = content$name,
                        description = content$description)
    synStore(file, activity = activity)
    unlink(content$output_file)
})
