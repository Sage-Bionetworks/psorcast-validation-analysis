library(synapser)
library(tidyverse)
library(data.table)

synapser::synLogin()


run_pip_width_ratio <- function(){
    rm(list=ls())
    gc()
    source("analysis/handImaging_analysis/swollen_joints/pip_swollen_joints.R")
    return(pip_swollen_effective_ratio)
}

run_dip_width_ratio <- function(){
    rm(list=ls())
    gc()
    source("analysis/handImaging_analysis/swollen_joints/dip_swollen_joints.R")
    return(dip_swollen_effective_ratio)
}


pip_ratio <- run_pip_width_ratio() %>%
    dplyr::select(pip_effective_ratio = value, everything())
dip_ratio <- run_dip_width_ratio() %>%
    dplyr::select(dip_effective_ratio = value, everything())

result <- pip_ratio %>% 
    dplyr::full_join(dip_ratio) %>%
    readr::write_tsv("finger_effective_ratio.tsv")

file <- synapser::File("finger_effective_ratio.tsv", 
                       parent = "syn22342373")
store <- synapser::synStore(file)





