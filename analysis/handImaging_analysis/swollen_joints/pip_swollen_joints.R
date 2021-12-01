########################################################################
# Psoriasis Validation
# Purpose: Compare swollen joints and pip/dip ratios across hands
# Author: Meghasyam Tummalacherla
# email: meghasyam@sagebase.org
########################################################################

##############
# Required libraries
##############
library(synapser)
library(githubr)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

synapser::synLogin()

##############
# Download data from Synapse and extract features/columns
##############
swollenJoints.tbl <- read.csv(synapser::synGet('syn25581019')$path, sep = '\t') %>% 
    dplyr::rename(image = name)
swollenJoints.tbl <- swollenJoints.tbl[grepl('_handImaging', swollenJoints.tbl$image),] 

swollenJoints.tbl.meta <- swollenJoints.tbl %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(index_pip_swollen = (left_pip_2 + right_pip_2),
                  middle_pip_swollen = (left_pip_3 + right_pip_3),
                  ring_pip_swollen = (left_pip_4 + right_pip_4),
                  pinky_pip_swollen = (left_pip_5 + right_pip_5)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(image, index_pip_swollen,
                  middle_pip_swollen,
                  ring_pip_swollen,
                  pinky_pip_swollen,)


# download data from synapse
finger.landmarks.syn.id <- 'syn26063507'

# POLYU landmarks
finger.landmarks.polyu <- synGet("syn26485445")$path %>%
    fread() %>% 
    dplyr::mutate(dataset = 'polyu') %>%
    dplyr::select(-V1)

# finger.landmarks.syn.id <- 'syn26171034'
finger.landmarks.psor <- read.csv(synapser::synGet(
    finger.landmarks.syn.id)$path) %>% 
    dplyr::filter(image %in% swollenJoints.tbl$image) %>% 
    dplyr::mutate(dataset = 'psorcast')

finger.landmarks <- finger.landmarks.psor %>% 
    dplyr::full_join(finger.landmarks.polyu)

finger.landmarks.sub.right <- finger.landmarks %>% 
    na.omit() %>% 
    dplyr::select(image,dataset,
                  index_pip_to_dip_length = right_index_pip_to_dip_length,
                  middle_pip_to_dip_length = right_middle_pip_to_dip_length,
                  ring_pip_to_dip_length = right_ring_pip_to_dip_length,
                  pinky_pip_to_dip_length = right_pinky_pip_to_dip_length,
                  index_pip_width = right_index_pip_width,
                  middle_pip_width = right_middle_pip_width,
                  ring_pip_width = right_ring_pip_width,
                  pinky_pip_width = right_pinky_pip_width) %>% 
    dplyr::mutate(hand = 'right') %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(avg_length = mean(c(index_pip_to_dip_length,
                                      middle_pip_to_dip_length,
                                      ring_pip_to_dip_length,
                                      pinky_pip_to_dip_length))) %>% 
    dplyr::mutate(effective_index_pip_width = index_pip_width/avg_length,
                  effective_middle_pip_width = middle_pip_width/avg_length,
                  effective_ring_pip_width = ring_pip_width/avg_length,
                  effective_pinky_pip_width = pinky_pip_width/avg_length) %>% 
    dplyr::mutate(effective_index_pip_width_2 = index_pip_width/index_pip_to_dip_length,
                  effective_middle_pip_width_2 = middle_pip_width/middle_pip_to_dip_length,
                  effective_ring_pip_width_2 = ring_pip_width/ring_pip_to_dip_length,
                  effective_pinky_pip_width_2 = pinky_pip_width/pinky_pip_to_dip_length) %>% 
    dplyr::ungroup() 

finger.landmarks.sub.left <- finger.landmarks %>% 
    na.omit() %>% 
    dplyr::select(image,dataset,
                  index_pip_to_dip_length = left_index_pip_to_dip_length,
                  middle_pip_to_dip_length = left_middle_pip_to_dip_length,
                  ring_pip_to_dip_length = left_ring_pip_to_dip_length,
                  pinky_pip_to_dip_length = left_pinky_pip_to_dip_length,
                  index_pip_width = left_index_pip_width,
                  middle_pip_width = left_middle_pip_width,
                  ring_pip_width = left_ring_pip_width,
                  pinky_pip_width = left_pinky_pip_width) %>% 
    dplyr::mutate(hand = 'left') %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(avg_length = mean(c(index_pip_to_dip_length,
                                      middle_pip_to_dip_length,
                                      ring_pip_to_dip_length,
                                      pinky_pip_to_dip_length))) %>% 
    dplyr::mutate(effective_index_pip_width = index_pip_width/avg_length,
                  effective_middle_pip_width = middle_pip_width/avg_length,
                  effective_ring_pip_width = ring_pip_width/avg_length,
                  effective_pinky_pip_width = pinky_pip_width/avg_length) %>% 
    dplyr::mutate(effective_index_pip_width_2 = index_pip_width/index_pip_to_dip_length,
                  effective_middle_pip_width_2 = middle_pip_width/middle_pip_to_dip_length,
                  effective_ring_pip_width_2 = ring_pip_width/ring_pip_to_dip_length,
                  effective_pinky_pip_width_2 = pinky_pip_width/pinky_pip_to_dip_length) %>% 
    dplyr::ungroup() 

# get ratio of left to right effective widths
finger.landmarks.sub <- finger.landmarks.sub.right %>% 
    dplyr::full_join(finger.landmarks.sub.left) %>% 
    dplyr::group_by(image) %>% 
    dplyr::mutate(effective_index_ratio = max(effective_index_pip_width)/min(effective_index_pip_width),
                  effective_middle_ratio = max(effective_middle_pip_width)/min(effective_middle_pip_width),
                  effective_ring_ratio = max(effective_ring_pip_width)/min(effective_ring_pip_width),
                  effective_pinky_ratio = max(effective_pinky_pip_width)/min(effective_pinky_pip_width)) %>% 
    dplyr::mutate(effective_index_ratio_2 = max(effective_index_pip_width_2)/min(effective_index_pip_width_2),
                  effective_middle_ratio_2 = max(effective_middle_pip_width_2)/min(effective_middle_pip_width_2),
                  effective_ring_ratio_2 = max(effective_ring_pip_width_2)/min(effective_ring_pip_width_2),
                  effective_pinky_ratio_2 = max(effective_pinky_pip_width_2)/min(effective_pinky_pip_width_2)) %>% 
    dplyr::mutate(index_ratio = max(index_pip_width)/min(index_pip_width),
                  middle_ratio = max(middle_pip_width)/min(middle_pip_width),
                  ring_ratio = max(ring_pip_width)/min(ring_pip_width),
                  pinky_ratio = max(pinky_pip_width)/min(pinky_pip_width)) %>% 
    dplyr::left_join(swollenJoints.tbl.meta)




aa <- tidyr::gather(finger.landmarks.sub, 'metric', 'value', -image,-hand, -avg_length, -dataset)




## required fns
is_pip_swollen <- function(metric, hand, left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                           right_pip_2, right_pip_3, right_pip_4, right_pip_5){
    
    
    if(hand == 'left'){
        if(grepl('index', metric)){
            if(is.na(left_pip_2)){
                return(NA)
            }else if(left_pip_2){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('middle', metric)){
            if(is.na(left_pip_3)){
                return(NA)
            }else if(left_pip_3){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('ring', metric)){
            if(is.na(left_pip_4)){
                return(NA)
            }else if(left_pip_4){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('pinky', metric)){
            if(is.na(left_pip_5)){
                return(NA)
            }else if(left_pip_5){
                return(1)
            }else{
                return(0)
            }
        }
        
    }else{
        if(grepl('index', metric)){
            if(is.na(right_pip_2)){
                return(NA)
            }else if(right_pip_2){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('middle', metric)){
            if(is.na(right_pip_3)){
                return(NA)
            }else if(right_pip_3){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('ring', metric)){
            if(is.na(right_pip_4)){
                return(NA)
            }else if(right_pip_4){
                return(1)
            }else{
                return(0)
            }
        }
        
        if(grepl('pinky', metric)){
            if(is.na(right_pip_5)){
                return(NA)
            }else if(right_pip_5){
                return(1)
            }else{
                return(0)
            }
        }
        
    }
    
}

################
### PIP 
################

### Checking left pip swollen joints with pip width, effective pip width and effective pip width ratio
pip_swollen <- aa %>% 
    dplyr::filter(grepl('_pip_width', metric)) %>% 
    dplyr::filter(!grepl('effective', metric)) %>% 
    # dplyr::filter(hand == 'left') %>% 
    dplyr::left_join(swollenJoints.tbl %>% 
                         dplyr::select(image, left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                       right_pip_2, right_pip_3, right_pip_4, right_pip_5,)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pip_swollen = is_pip_swollen(metric,hand,left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                               right_pip_2, right_pip_3, right_pip_4, right_pip_5)) %>% 
    dplyr::ungroup()

ggplot2::ggplot(pip_swollen,aes(x = metric, y = value, fill = dataset, color = as.factor(pip_swollen))) + geom_boxplot() +
    facet_wrap(~hand, scales = 'free')+
    ylim(c(25,135))
## Raw image widths show some promise

# Now checking to see if any effective widths are picking this up?
pip_swollen_effective <-  aa %>% 
    dplyr::filter(grepl('_pip_width', metric)) %>% 
    dplyr::filter(grepl('effective', metric)) %>% 
    # dplyr::filter(hand == 'left') %>% 
    dplyr::left_join(swollenJoints.tbl %>% 
                         dplyr::select(image, left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                       right_pip_2, right_pip_3, right_pip_4, right_pip_5,)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pip_swollen = is_pip_swollen(metric,hand,left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                               right_pip_2, right_pip_3, right_pip_4, right_pip_5)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(value > 0) %>% 
    dplyr::filter(value < 2) # do not expect to see dip more than 2 times pip

ggplot2::ggplot(pip_swollen_effective,aes(y = value, fill = dataset, color = as.factor(pip_swollen))) +
    # ylim(c(0.5,2))+
    facet_wrap(~metric+hand, scales = 'free')+
    geom_boxplot() 

# Now checking to see if any effective ratios are picking this up?
pip_swollen_effective_ratio <-  aa %>% 
    dplyr::filter(grepl('_ratio', metric)) %>% 
    dplyr::filter(grepl('effective', metric)) %>% 
    # dplyr::filter(hand == 'left') %>% 
    dplyr::left_join(swollenJoints.tbl %>% 
                         dplyr::select(image, left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                       right_pip_2, right_pip_3, right_pip_4, right_pip_5,)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pip_swollen = is_pip_swollen(metric,hand,left_pip_2, left_pip_3, left_pip_4, left_pip_5,
                                               right_pip_2, right_pip_3, right_pip_4, right_pip_5)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(value > 0) %>% 
    dplyr::filter(value < 2) # do not expect to see dip more than 2 times pip

ggplot2::ggplot(pip_swollen_effective_ratio,aes(y = value, fill = dataset, color = as.factor(pip_swollen))) +
    # ylim(c(0.5,2))+
    facet_wrap(~metric+hand, scales = 'free')+
    geom_boxplot() 


########## for PIP #######
# Effective widths (for each hand) are better use case than effective ratios (across hands)
##########################

########## for DIP #######
# NOT AT ALL WORKING FOR DIP!!!!