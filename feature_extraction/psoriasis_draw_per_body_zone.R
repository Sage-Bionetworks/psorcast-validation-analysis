########################################################################
# Psoriasis Validation Analysis
# Purpose: To get percent of pixels drawn per body zone in Psoriasis Draw
# Authors: Meghasyam Tummalacherla, Aryton Tediarjos
# Maintainers: meghasyam@sagebase.org, aryton.tediarjo@sagebase.org
########################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(tidyverse)
library(synapser)
library(magick)
library(data.table)
library(githubr)
synapser::synLogin()
source("utils/feature_extraction_utils.R")


#' get visit reference and curated ppacman table
PPACMAN_TBL_ID <- "syn22337133"
VISIT_REF_ID <- "syn25825626"
visit_ref <- synGet(VISIT_REF_ID)$path %>% fread()
ppacman <- synGet(PPACMAN_TBL_ID)$path %>% fread() %>%
    dplyr::select(participantId,
                  createdOn,
                  visit_num, 
                  diagnosis)

############################
# Git Reference
############################
SCRIPT_PATH <- file.path('feature_extraction', "psoriasis_draw_per_body_zone.R")
GIT_TOKEN_PATH <- config::get("git")$token_path
GIT_REPO <- config::get("git")$repo
githubr::setGithubToken(readLines(GIT_TOKEN_PATH))
GIT_URL <- getPermlink(
    repository = getRepo(
        repository = GIT_REPO, 
        ref="branch", 
        refName='master'), 
    repositoryPath = SCRIPT_PATH)

##############
# Required functions
##############
resizeImageToSpec <- function(im_, specX = 683, specY = 656){
    # resize input image(magick image) to specX x specY, ignoring aspect ratio
    # 683, 656 are image dimensions of corrected ref image (syn25860579)
    return(magick::image_resize(im_, paste0(specX,'x',specY,'!')))
}

removePadding <- function(im_){
    # resize input image(magick image) to specX x specY, ignoring aspect ratio
    im.mat <- magick::image_data(im_)
    imin = NULL
    imax = NULL
    jmin = NULL
    jmax = NULL
    
    for(i in seq(dim(im.mat)[2])){
        for(j in seq(dim(im.mat)[3])){
            if((im.mat[3,i,j] == 209)){
                
                if(is.null(imin)){
                    imin = i
                }else{
                    if(i < imin){
                        imin = i
                    }
                    
                    if(is.null(imax)){
                        imax = i
                    }else{
                        if(i > imax){
                            imax = i
                        }
                    }    
                    
                    if(is.null(jmin)){
                        jmin = j
                    }else{
                        if(j < jmin){
                            jmin = j
                        }
                        
                        if(is.null(jmax)){
                            jmax = j
                        }else{
                            if(j > jmax){
                                jmax = j
                            }
                        }    
                        
                        
                        
                    }
                }
            }
        }
    }
    aa <- im.mat[,imin:imax, jmin:jmax]
    return(magick::image_read(aa))
}

isolate_psoriasis_red <- function(im_){
    #isolate the pixels that signal drawing on the body and change to 1.0; all else 0.0
    im_dat <- magick::image_data(im_)
    temp_im <- im_dat
    im_shape <- dim(im_dat[1,,])
    nRow <- im_shape[1] # has to be 775 since all images are normalzied
    nCol <- im_shape[2] # has to be 775 since all images are normalzied
    
    for (i in seq(nRow)){
        for (j in seq(nCol)){
            blue_ <- im_dat[1,i,j]
            green_ <- im_dat[2,i,j]
            red_ <- im_dat[3,i,j]
            if(red_ > 100 && red_ < 150){
                temp_im[3,i,j] <- as.raw(255)
                temp_im[2,i,j] <- as.raw(255)
                temp_im[1,i,j] <- as.raw(255)
            }else{
                temp_im[3,i,j] <- as.raw(0)
                temp_im[2,i,j] <- as.raw(0)
                temp_im[1,i,j] <- as.raw(0)
            }
        }
    }
    return(temp_im)
}

isolate_background_mask <- function(im_){
    #isolate the pixels that belong to the background (all white)
    im_dat <- magick::image_data(im_)
    temp_im <- im_dat
    im_shape <- dim(im_dat[1,,])
    nRow <- im_shape[1] # has to be 775 since all images are normalzied
    nCol <- im_shape[2] # has to be 775 since all images are normalzied
    
    for (i in seq(nRow)){
        for (j in seq(nCol)){
            blue_ <- im_dat[1,i,j]
            green_ <- im_dat[2,i,j]
            red_ <- im_dat[3,i,j]
            if(red_ > as.raw(240) & green_ > as.raw(240) & blue_ > as.raw(240)){
                temp_im[3,i,j] <- as.raw(255)
                temp_im[2,i,j] <- as.raw(255)
                temp_im[1,i,j] <- as.raw(255)
            }else{
                temp_im[3,i,j] <- as.raw(0)
                temp_im[2,i,j] <- as.raw(0)
                temp_im[1,i,j] <- as.raw(0)
            }
        }
    }
    return(temp_im)
}

getPercentShaded <- function(redPixel_isolated_im_dat, background_mask_dat){
    # function to get shaded area percentage
    # redPixel_isolated_im_dat and background_mask_dat are of same dimensions
    dim_im <- dim(redPixel_isolated_im_dat)
    totalPixel <- dim_im[1]*dim_im[2]*dim_im[3]
    shadeWhitePixel <- sum(redPixel_isolated_im_dat == as.raw(255)) # get all ff (white) pixels
    shadeBlackPixel <- sum(redPixel_isolated_im_dat == as.raw(0)) # get all 00 (blacks) pixels
    
    backgroundPixel <- sum(background_mask_dat == as.raw(255))
    
    # parity check for redPixel_isolated_im_dat
    parityCheck = (totalPixel == (shadeWhitePixel + shadeBlackPixel))
    if(parityCheck){
        # (totalPixel - backgroundPixel) is the amount of pixels the users can actually shade 
        shadePercent = shadeWhitePixel/(totalPixel - backgroundPixel)*100
    }else{
        shadePercent = NA
    }
    return(shadePercent)
}

getPercentShadedPerZone <- function(im_, recordId_, meta.info){
    # Given an image output a dataframe with zones as columns
    # and shaded percent as value in columns
    # im_ is resized magick image
    # recordId_ is the recordId/ unique ID for this image
    # meta.info is the dataframe containing info about zones(Zone,x,y,w,h)
    
    meta.info$Zone <- as.character(meta.info$Zone)
    
    im_list <- list()
    for(i in seq(nrow(meta.info))){
        # print(meta.info$Zone[i])
        geometry.info <- paste0(meta.info$w[i], 'x',
                                meta.info$h[i], '+',
                                meta.info$x[i], '+',
                                meta.info$y[i])
        aa <- magick::image_crop(im_, geometry.info)
        aa_red <- isolate_psoriasis_red(aa)
        aa_back <- isolate_background_mask(aa)
        
        im_list[[i]] <- data.frame(zone = meta.info$Zone[i],
                                   percentShaded= getPercentShaded(aa_red, aa_back)) %>% 
            dplyr::mutate(recordId = recordId_)
        
    }
    
    out.dat <- data.table::rbindlist(im_list)
    out.dat <- tidyr::spread(out.dat, zone, percentShaded)
    return(out.dat)
    
}

##############
# Download data from Synapse
##############
# Download psoriasis draw images tbl from Synaspe
psoriasisDraw.tbl.id <- 'syn22281746'
psoriasisDraw.tbl.syn <- synapser::synTableQuery(paste0('SELECT * FROM ', psoriasisDraw.tbl.id))
psoriasisDraw.tbl <- psoriasisDraw.tbl.syn$asDataFrame()
all.used.ids <- psoriasisDraw.tbl.id

# get the draw zone reference image
# getting the corrected ref Image
# for actual ref Image look at syn25731429
drawZone.ref.image.id <- 'syn25860579'
drawZone.ref.image <- magick::image_read(synapser::synGet(drawZone.ref.image.id)$path)
all.used.ids <- c(all.used.ids, drawZone.ref.image.id)

# get draw zone coordinates info for ref image 
psoriasisDrawZones.info.id <- 'syn25860325'
psoriasisDrawZones.info <- read.csv(synapser::synGet(psoriasisDrawZones.info.id)$path) %>% 
    dplyr::select(Zone, x = x2, y = y2, w = w2, h = h2)
all.used.ids <- c(all.used.ids, psoriasisDrawZones.info.id)

# Download curated psoriasis draw images list from synapse
psoriasisDraw.images.id <- 'syn25837658'
psoriasisDraw.images.tbl <- synapser::synGetChildren(psoriasisDraw.images.id)$asList() %>% 
    data.table::rbindlist(fill = T) %>% 
    dplyr::filter(grepl('.jpg',name)) 
all.used.ids <- c(all.used.ids, psoriasisDraw.images.id)

psoriasisDraw.images.tbl.metadata <- apply(psoriasisDraw.images.tbl,1, function(x){
    temp.list <- synapser::synGetAnnotations(x[['id']]) 
    temp.list$id <- x[['id']]
    return(temp.list)
} ) %>% data.table::rbindlist(fill = T)

psoriasisDraw.images.tbl.metadata$recordId <- as.character(psoriasisDraw.images.tbl.metadata$recordId)
psoriasisDraw.images.tbl.metadata$createdOn <- as.character(psoriasisDraw.images.tbl.metadata$createdOn)
psoriasisDraw.images.tbl.metadata$participantId <- as.character(psoriasisDraw.images.tbl.metadata$participantId)

psoriasisDraw.images.meta.tbl <- psoriasisDraw.images.tbl %>% 
    dplyr::left_join(psoriasisDraw.images.tbl.metadata %>% 
                         dplyr::select(-createdOn))

# Download and update local file paths for the images
psoriasisDraw.images.meta.tbl <- psoriasisDraw.images.meta.tbl %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(localPath = synapser::synGet(id)$path) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(recordId %in% psoriasisDraw.tbl$recordId) %>% 
    dplyr::mutate(imageInfo = magick::image_info(magick::image_read(localPath)))

##############
# Calculate shaded percent per zone per image
##############
psoriasisDraw.tbl.meta.noNA <- psoriasisDraw.images.meta.tbl[!is.null(psoriasisDraw.images.meta.tbl$recordId)]
percentShaded.tbl <- apply(psoriasisDraw.tbl.meta.noNA,1,function(x){
    print(x[['recordId']])
    
    # read image
    im <- magick::image_read(x[['localPath']])
    # remove padding
    im2 <- removePadding(im)
    # resize image
    im3 <- resizeImageToSpec(im2)
    # get shaded percent per zone
    percentShadedZone <- getPercentShadedPerZone(im_ = im3,
                                                 recordId_ = x[['recordId']],
                                                 meta.info = psoriasisDrawZones.info)
    return(percentShadedZone)}) %>% data.table::rbindlist(fill = T) %>% 
    dplyr::left_join(psoriasisDraw.tbl.meta.noNA %>% 
                         dplyr::select(recordId, participantId, createdOn))



######
## ARYTON to add diagnosis from PPACMAN table
######
percentShaded.tbl.joined.ppacman <- 
    psoriasisDraw.images.tbl.metadata %>% 
    dplyr::select(recordId, createdOn) %>%
    dplyr::mutate(createdOn = lubridate::as_datetime(createdOn)) %>%
    dplyr::left_join(percentShaded.tbl %>% 
                         dplyr::select(-createdOn), 
                     by = "recordId") %>%
    join_with_ppacman(visit_ref, ppacman)


## Upload new table to Synapse
synapse.folder.id <- 'syn22336716' # synId of folder to upload your file to
OUTPUT_FILE <- 'psoriasisDrawPerZone_features.tsv' # name your file
write.table(percentShaded.tbl.joined.ppacman, 
            OUTPUT_FILE, sep="\t", 
            row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         used = all.used.ids,
         executed = GIT_URL)
unlink(OUTPUT_FILE)

## TO-DO
# 3. Should we have a column that states how much area is from this region? (can do this later)
# t-test on image distribution
