###########
## Curate 11K Hands
###########

###########
# libraries
###########
library(tidyverse)
library(synapser)
library(magick)
synapser::synLogin()

###########
# Read metadata file, and merge left, right images based in id
###########
metadata.info <- read.csv('HandInfo.csv')
metadata.dorsal <- metadata.info %>% 
    dplyr::filter(grepl('dorsal', aspectOfHand)) %>% 
    droplevels() %>% 
    dplyr::group_by(id, aspectOfHand) %>% 
    dplyr::mutate(Order = rank(imageName)) %>% 
    dplyr::ungroup()

metadata.dorsal.right <- metadata.dorsal %>% 
    dplyr::filter(grepl('right', aspectOfHand)) %>% 
    dplyr::rename(accessoriesRight = accessories,
                  nailPolishRight = nailPolish,
                  aspectOfHandRight = aspectOfHand,
                  imageNameRight = imageName,
                  irregularitiesRight = irregularities)

metadata.dorsal.left <- metadata.dorsal %>% 
    dplyr::filter(grepl('left', aspectOfHand)) %>% 
    dplyr::rename(accessoriesLeft = accessories,
                  nailPolishLeft = nailPolish,
                  aspectOfHandLeft = aspectOfHand,
                  imageNameLeft = imageName,
                  irregularitiesLeft = irregularities)

metadata.matched.all <- metadata.dorsal.right %>% 
    dplyr::full_join(metadata.dorsal.left) 

metadata.matched <- metadata.matched.all %>% 
    na.omit() %>% 
    dplyr::mutate(mergedImage = 'placeHolder')

############
# Read, merge and write images
############
for(i in seq(nrow(metadata.matched))){
    print(i)
    imageLeft <- magick::image_read(paste0('Hands/', as.character(metadata.matched$imageNameLeft[i])))
    imageRight <- magick::image_read(paste0('Hands/', as.character(metadata.matched$imageNameRight[i])))
    imageSummary <- image_append(c(imageRight, imageLeft)) %>% 
        image_rotate(180)
    imageName <- paste0(str_replace(metadata.matched$imageNameLeft[i], '.jpg',''),str_replace(metadata.matched$imageNameRight[i], '.jpg',''))
    image_write(imageSummary, path = paste0('HandsCurated/',imageName, '.jpg'), format = 'jpg')
    metadata.matched$mergedImage[i] <- paste0(imageName,'.jpg')
}

###########
# Upload data to Synapse
###########
OUTPUT_FILE <- "mergedMetadataCurated.tsv" # name your file
write.table(metadata.matched, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId='syn25517068'))
unlink(OUTPUT_FILE)

OUTPUT_FILE <- "metadataDorsalAll.tsv" # name your file
write.table(metadata.dorsal, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId='syn25517068'))
unlink(OUTPUT_FILE)

for(i in seq(nrow(metadata.matched))){
    outfile <- synapser::synStore(File(paste0('HandsCurated/', metadata.matched$mergedImage[i]),
                                       parentId='syn25517084'))
    annotations_list = list(
        subject_id = metadata.matched$id[i] %>% as.character(),
        age = metadata.matched$age[i],
        gender = metadata.matched$gender[i] %>% as.character(),
        skinColor = metadata.matched$skinColor[i] %>% as.character(),
        accessoriesRight = metadata.matched$accessoriesRight[i],
        accessoriesLeft = metadata.matched$accessoriesLeft[i],
        nailPolishRight = metadata.matched$nailPolishRight[i],
        nailPolishLeft = metadata.matched$nailPolishLeft[i],
        irregularitiesRight = metadata.matched$irregularitiesRight[i],
        irregularitiesLeft = metadata.matched$irregularitiesLeft[i]
    )
    synSetAnnotations(outfile$properties$id, 
                      annotations = annotations_list)
    print(i)
}