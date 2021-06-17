####################################################################################
#' About: This script is used as a master file for compiling
#' all .Rmd knitting process to Synapse for the Psorcast project
#' Author: Aryton Tediarjo (aryton.tediarjo@sagebase.org)
#' TODO: think on how to add provenance for compiling all the scripts
####################################################################################

#####################
# load library
####################
library(synapser)
library(knit2synapse)
library(data.table)
library(githubr)
library(dplyr)
library(purrr)
library(config)
source('utils/processing_log_utils.R')
synLogin()

########################
# global variables
#######################
NOTEBOOK_REFS <- list(
    demographics = list(
        used = 'syn22337133',
        parent = "syn22337138",
        path = "analysis/demographics_analysis/demographics_eda.Rmd",
        fileName = "Demographics EDA"
    ),
    jar_opener = list(
        used = c('syn25006883', 'syn25830490', 'syn22337137'),
        parent = "syn22337138",
        path = "analysis/digJarOpener_analysis/jarOpen_eda.Rmd",
        fileName = "Digital Jar Opener EDA"
    ),
    jar_opener_v2 = list(
        used = c('syn25832975'),
        parent = "syn22337138",
        path = "analysis/digJarOpener_analysis/jarOpen_eda_v2.Rmd",
        fileName = "Digital Jar Opener EDA - Extended"
    ),
    joint_counts = list(
            used = c('syn25832975'),
            parent = "syn22337138",
            path = "analysis/jointCounts_analysis/jcounts_EDA.Rmd",
            fileName = "Joint Counts EDA"
        ),
    joint_counts = list(
        used = c('syn25832975'),
        parent = "syn22337138",
        path = "analysis/psoDraw_analysis/psodraw_BSA_EDA.Rmd",
        fileName = "Psoriasis Draw BSA EDA"
    ),
    walk = list(
        used = c('syn25832975'),
        parent = "syn22337138",
        path = "analysis/walk30secs_analysis/walk30secs_EDA.Rmd",
        fileName = "Walk 30s EDA"
    )
)

############################
# Git Reference
############################
SCRIPT_PATH <- file.path('analysis', "knit_markdowns.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))


############################
# Helpers
############################
get_git_url <- function(git_repo, script_path){
     githubr::getPermlink(
        repository = githubr::getRepo(
            repository = git_repo, 
            ref="branch", 
            refName='main'), 
        repositoryPath = script_path)
    
}


main <- function(){
    NOTEBOOK_REFS %>% 
        purrr::walk(
            function(notebook){
                knit2synapse::createAndKnitToFileEntity(
                    file = notebook$path,
                    parentId = notebook$parent,
                    fileName = notebook$fileName,
                    used = notebook$used,
                    executed = c(get_git_url(GIT_REPO, notebook$path), 
                                 get_git_url(GIT_REPO, SCRIPT_PATH)))
            }
        )
}

log_process(main(), SCRIPT_PATH)


