## performs an extratified subject-wise train/test
## split (obtain routhly the same proportion of 
## positive and negative cases in the training and 
## test sets, but at the same time makes sure that
## the data of individuals that have multiple records
## is either in the training or in the test sets)
ExtratifiedSubjectwiseSplit <- function(dat,
                                        label.name,
                                        subject.id.name,
                                        neg.class.name, 
                                        pos.class.name,
                                        train.prop = 0.5) {
    dat$subjectId_and_label <- paste(dat[, subject.id.name], dat[, label.name], sep = " _ JOIN _ ")
    aux1 <- unique(dat$subjectId_and_label)
    aux2 <- strsplit(aux1, " _ JOIN _ ")
    aux3 <- unlist(lapply(aux2, function(x) x[2]))
    
    idx.N <- which(aux3 == neg.class.name)
    idx.P <- which(aux3 == pos.class.name)
    n.N <- length(idx.N)
    n.P <- length(idx.P)
    i.train <- c(sample(idx.N, round(n.N * train.prop), replace = FALSE),
                 sample(idx.P, round(n.P * train.prop), replace = FALSE))
    i.test <- setdiff(seq(n.N + n.P), i.train)
    
    aux1.train <- aux1[i.train]
    aux1.test <- aux1[i.test]
    idx.train <- which(dat$subjectId_and_label %in% aux1.train)
    idx.test <- which(dat$subjectId_and_label %in% aux1.test)
    
    list(idx.train = idx.train, idx.test = idx.test)
}

## runs the experiments on the original, adjusted, and shuffled
## data for the label classification
RunGlmCA <- function(n.runs,
                     dat,
                     label.name, 
                     feature.names,
                     confounder.names,
                     subject.id.name,
                     neg.class.name, 
                     pos.class.name,
                     train.prop = 0.5,
                     my.seed) {
    original.outputs <- vector(mode = "list", length = n.runs)
    adjusted.outputs <- vector(mode = "list", length = n.runs)
    AUROCs <- matrix(NA, n.runs, 3)
    colnames(AUROCs) <- c("original", "adjusted", "shuffled")
    AUROCs.pvals <- AUROCs
    AUPRCs <- AUROCs
    n.confounders <- length(confounder.names)
    pcors <- vector(mode = "list", length(n.confounders))
    for (i in seq(n.confounders)) {
        aux <- matrix(NA, n.runs, 10)
        colnames(aux) <- c("cor(R,Y)", "cor(R,C)", "cor(C,Y)", "cor(R,Y|C)", "cor(R,C|Y)",
                           "pval(R,Y)", "pval(R,C)", "pval(C,Y)", "pval(R,Y|C)", "pval(R,C|Y)")
        pcors[[i]] <- aux
    }
    names(pcors) <- confounder.names
    pcors_c <- pcors
    
    dat <- dat[, c(subject.id.name, label.name, feature.names, confounder.names, "visit_num")]
    dat <- na.omit(dat)
    n <- nrow(dat)
    
    set.seed(my.seed) 
    my.seeds <- sample(seq(1e+4, 1e+5), n.runs, replace = FALSE)
    
    for (i in seq(n.runs)) {
        cat(i, "\n")
        
        set.seed(my.seeds[i])
        aux.split <- ExtratifiedSubjectwiseSplit(dat, 
                                                 label.name, 
                                                 subject.id.name,
                                                 neg.class.name, 
                                                 pos.class.name, 
                                                 train.prop)
        idx.train <- aux.split$idx.train
        idx.test <- aux.split$idx.test
        
        ## causality-aware adjusted
        dat2 <- CausalityAwareAdjustment(dat = dat, 
                                         idx.train = idx.train, 
                                         idx.test = idx.test, 
                                         label.name = label.name, 
                                         feature.names = feature.names, 
                                         confounder.name = confounder.names)
        
        ################################################
        ## label classification
        ################################################
        
        ## original data
        aux1 <- FitGlm(dat,
                       idx.train, 
                       idx.test, 
                       label.name = label.name, 
                       feature.names = feature.names,
                       neg.class.name = neg.class.name, 
                       pos.class.name = pos.class.name)
        AUROCs[i, "original"] <- aux1$auroc
        AUROCs.pvals[i, "original"] <- aux1$pval
        AUPRCs[i, "original"] <- aux1$auprc
        for (j in seq(n.confounders)) {
            pcors1 <- GetPCorPatterns(Yhat = aux1$pred.probs, 
                                      Y = dat[idx.test, label.name],
                                      A = dat[idx.test, confounder.names[j]])
            
            pcors[[j]][i, 1:5] <- pcors1$cors_A[1:5]
            pcors[[j]][i, 6:10] <- pcors1$cors_A[6:10]
        }
        tmp <- dat[idx.test, c("participantId", "visit_num", label.name)]
        tmp$pred.prob <- aux1$pred.probs
        original.outputs[[i]] <- list(test.data = tmp, 
                                      auroc.obj = aux1$auroc.obj,
                                      auprc.obj = aux1$auprc.obj)
        
        ## shuffled data
        pdat <- dat
        pdat[, label.name] <- dat[sample(n), label.name]
        aux1 <- FitGlm(pdat,
                       idx.train, 
                       idx.test, 
                       label.name = label.name, 
                       feature.names = feature.names,
                       neg.class.name = neg.class.name, 
                       pos.class.name = pos.class.name)
        AUROCs[i, "shuffled"] <- aux1$auroc
        AUROCs.pvals[i, "shuffled"] <- aux1$pval
        AUPRCs[i, "shuffled"] <- aux1$auprc
        
        ## adjusted data
        aux2 <- FitGlm(dat2,
                       idx.train, 
                       idx.test, 
                       label.name, 
                       feature.names,
                       neg.class.name, 
                       pos.class.name)
        AUROCs[i, "adjusted"] <- aux2$auroc
        AUROCs.pvals[i, "adjusted"] <- aux2$pval
        AUPRCs[i, "adjusted"] <- aux2$auprc
        for (j in seq(n.confounders)) {
            pcors2 <- GetPCorPatterns(Yhat = aux2$pred.probs, 
                                      Y = dat[idx.test, label.name],
                                      A = dat[idx.test, confounder.names[j]])
            
            pcors_c[[j]][i, 1:5] <- pcors2$cors_A[1:5]
            pcors_c[[j]][i, 6:10] <- pcors2$cors_A[6:10]
        }
        tmp2 <- dat2[idx.test, c("participantId", "visit_num", label.name)]
        tmp2$pred.prob <- aux2$pred.probs ## replace the predicted prob
        adjusted.outputs[[i]] <- list(test.data = tmp2, 
                                      auroc.obj = aux2$auroc.obj,
                                      auprc.obj = aux2$auprc.obj)
    }
    
    list(AUROCs = AUROCs,
         AUPRCs = AUPRCs,
         AUROCs.pvals = AUROCs.pvals,
         pcors = pcors,
         pcors_c = pcors_c,
         original.outputs = original.outputs,
         adjusted.outputs = adjusted.outputs)
}


## checks whether the adjusted input is no longer
## able to predict the confounder
AgeAdjustmentSanityCheck <- function(n.runs,
                                     dat,
                                     label.name, 
                                     feature.names,
                                     confounder.names,
                                     subject.id.name,
                                     neg.class.name, 
                                     pos.class.name,
                                     train.prop = 0.5,
                                     my.seed) {
    CCCs.age <- matrix(NA, n.runs, 3)
    colnames(CCCs.age) <- c("original", "adjusted", "shuffled")
    dat <- dat[, c(subject.id.name, label.name, feature.names, confounder.names, "visit_num")]
    dat <- na.omit(dat)
    n <- nrow(dat)
    
    set.seed(my.seed) 
    my.seeds <- sample(seq(1e+4, 1e+5), n.runs, replace = FALSE)
    
    for (i in seq(n.runs)) {
        cat(i, "\n")
        
        set.seed(my.seeds[i]) 
        aux.split <- ExtratifiedSubjectwiseSplit(dat, 
                                                 label.name, 
                                                 subject.id.name,
                                                 neg.class.name, 
                                                 pos.class.name, 
                                                 train.prop)
        idx.train <- aux.split$idx.train
        idx.test <- aux.split$idx.test
        
        ## causality-aware adjusted
        dat2 <- CausalityAwareAdjustment(dat = dat, 
                                         idx.train = idx.train, 
                                         idx.test = idx.test, 
                                         label.name = label.name, 
                                         feature.names = feature.names, 
                                         confounder.name = confounder.names)
        ## original data
        aux1 <- FitLm(dat,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "original"] <- aux1$ccc.obs      
        
        ## adjusted data
        aux2 <- FitLm(dat2,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "adjusted"] <- aux2$ccc.obs   
        
        ## shuffled data
        pdat <- dat
        pdat[, "age"] <- dat[sample(n), "age"]
        aux1 <- FitLm(pdat,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "shuffled"] <- aux1$ccc.obs 
    }
    
    list(CCCs.age = CCCs.age)
}


## checks whether our inputs can be used to predict 
## the age, gender, and site confounders
InformalChecks <- function(n.runs,
                           dat,
                           feature.names,
                           subject.id.name,
                           train.prop = 0.5,
                           my.seed) {
    CCCs.age <- matrix(NA, n.runs, 2)
    colnames(CCCs.age) <- c("original", "shuffled")
    AUROCs.sex <- CCCs.age
    AUROCs.site <- CCCs.age
    AUROCs.sex.pvals <- CCCs.age
    AUROCs.site.pvals <- CCCs.age
    AUPRCs.sex <- CCCs.age
    AUPRCs.site <- CCCs.age
    dat <- dat[, c(subject.id.name, feature.names, "combined_upper_pain", "age", "sex", "site", "visit_num")]
    dat <- na.omit(dat)
    n <- nrow(dat)
    
    set.seed(my.seed) 
    my.seeds <- sample(seq(1e+4, 1e+5), n.runs, replace = FALSE)
    
    for (i in seq(n.runs)) {
        cat(i, "\n")
        
        set.seed(my.seeds[i])
        aux.split <- ExtratifiedSubjectwiseSplit(dat, 
                                                 label.name = "combined_upper_pain", 
                                                 subject.id.name,
                                                 neg.class.name = "FALSE", 
                                                 pos.class.name = "TRUE", 
                                                 train.prop)
        idx.train <- aux.split$idx.train
        idx.test <- aux.split$idx.test
        
        ################################################
        ## gender classification
        ################################################
        ## original data
        aux1 <- FitGlm(dat,
                       idx.train, 
                       idx.test, 
                       label.name = "sex", 
                       feature.names = feature.names,
                       neg.class.name = "Female", 
                       pos.class.name = "Male")
        AUROCs.sex[i, "original"] <- aux1$auroc
        AUROCs.sex.pvals[i, "original"] <- aux1$pval
        AUPRCs.sex[i, "original"] <- aux1$auprc
        ## shuffled data
        pdat <- dat
        pdat[, "sex"] <- dat[sample(nrow(dat)), "sex"]
        aux1 <- FitGlm(pdat,
                       idx.train, 
                       idx.test, 
                       label.name = "sex", 
                       feature.names = feature.names,
                       neg.class.name = "Female", 
                       pos.class.name = "Male")
        AUROCs.sex[i, "shuffled"] <- aux1$auroc
        AUROCs.sex.pvals[i, "shuffled"] <- aux1$pval
        AUPRCs.sex[i, "shuffled"] <- aux1$auprc
        
        ################################################
        ## site classification
        ################################################
        ## original data
        aux1 <- FitGlm(dat,
                       idx.train, 
                       idx.test, 
                       label.name = "site", 
                       feature.names = feature.names,
                       neg.class.name = "SITE1", 
                       pos.class.name = "SITE2")
        AUROCs.site[i, "original"] <- aux1$auroc
        AUROCs.site.pvals[i, "original"] <- aux1$pval
        AUPRCs.site[i, "original"] <- aux1$auprc
        ## shuffled data
        pdat <- dat
        pdat[, "site"] <- dat[sample(nrow(dat)), "site"]
        aux1 <- FitGlm(pdat,
                       idx.train, 
                       idx.test, 
                       label.name = "site", 
                       feature.names = feature.names,
                       neg.class.name = "SITE1", 
                       pos.class.name = "SITE2")
        AUROCs.site[i, "shuffled"] <- aux1$auroc
        AUROCs.site.pvals[i, "shuffled"] <- aux1$pval
        AUPRCs.site[i, "shuffled"] <- aux1$auprc
        
        ##############################################
        ## age prediction
        ##############################################
        ## original data
        aux1 <- FitLm(dat,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "original"] <- aux1$ccc.obs      
        ## shuffled data
        pdat <- dat
        pdat[, "age"] <- dat[sample(nrow(dat)), "age"]
        aux1 <- FitLm(pdat,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "shuffled"] <- aux1$ccc.obs 
    }
    
    list(CCCs.age = CCCs.age,
         AUROCs.sex = AUROCs.sex,
         AUPRCs.sex = AUPRCs.sex,
         AUROCs.site = AUROCs.site,
         AUPRCs.site = AUPRCs.site,
         AUROCs.sex.pvals = AUROCs.sex.pvals,
         AUROCs.site.pvals = AUROCs.site.pvals)
}


## generates site variable (anything different from SITE1 is SITE2)
GenerateSiteVariable <- function(dat,
                                 subject.id.name = "participantId") {
    aux <- strsplit(as.character(dat[, subject.id.name]), "_")
    aux <- unlist(lapply(aux, function(x) x[1]))
    aux[aux != "site2"] <- "SITE1"
    aux[aux == "site2"] <- "SITE2"
    dat$site <- as.factor(aux)
    
    dat
}

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

fetch_results <- function(metrics_list, glue_label, key){
    row_loc <- list(
        combined = metrics_list$auc_iteration %>% 
            get_med_auc_info() %>%
            dplyr::mutate(auc_string = glue::glue(
                "AUC: {md_auc}"))
    )
    tpr_fpr <- 
        rocit(
            score=metrics_list$adjusted_outputs[[row_loc$combined$n_row]]$test.data$pred.prob,
            class=metrics_list$adjusted_outputs[[row_loc$combined$n_row]]$test.data[[key]])
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
    
    return(list(
        md_fpr_tpr = result,
        auc_iter = metrics_list$auc_iteration))
}


#####################################################
## shape the data for the analysis
#####################################################

require(synapser)
synLogin()

dat0 <- read.delim(synGet("syn26148414")$path, header = TRUE, quote = "")
dim(dat0)

#####################################################
## get dataset for the "informal checks" and and 
## combined-upper-pain analyses
#####################################################

## add site variable
dat1 <- GenerateSiteVariable(dat = dat0) 
dim(dat1)

#####################################################
## get dataset for the PsA vs PsO analyses
#####################################################

dat2 <- dat1
dat2 <- dat2[dat2$combined_upper_pain == "FALSE",]
dat2 <- dat2[dat2$diagnosis != "Control",]
dat2$PsA <- as.character(dat2$diagnosis)
dat2[which(dat2$PsA != "PsA"), "PsA"] <- "FALSE"
dat2[which(dat2$PsA == "PsA"), "PsA"] <- "TRUE"
table(dat2$PsA)

table(dat2$PsA, dat2$diagnosis)
table(dat1$combined_upper_pain, dat1$diagnosis)
table(dat2$combined_upper_pain, dat2$diagnosis)



################################
## run informal checks
################################

ic <- InformalChecks(n.runs = 1000,
                     dat = dat1,
                     feature.names = "total_rotation",
                     subject.id.name = "participantId",
                     train.prop = 0.5,
                     my.seed = 12345)


#####################################################
## run combined-upper-pain and PsA vs PsO analyses
#####################################################

## run combined-upper-pain analyses
c1 <- RunGlmCA(n.runs = 1000,
               dat = dat1,
               label.name = "combined_upper_pain", 
               feature.names = "total_rotation", 
               confounder.names = "age",
               subject.id.name = "participantId",
               neg.class.name = "FALSE", 
               pos.class.name = "TRUE",
               train.prop = 0.5,
               my.seed = 12345)

apply(c1$AUROCs, 2, median)


AUROCs <- c1$AUROCs
AUPRCs <- c1$AUPRCs
original.outputs <- c1$original.outputs
adjusted.outputs <- c1$adjusted.outputs


metrics_list <- list(
    adjusted_outputs = adjusted.outputs,
    auc_iteration = AUROCs %>%
        tibble::as_tibble()
)
#save(AUROCs, AUPRCs, original.outputs, adjusted.outputs, file = "psorcast_outputs_updated_combined_upper_pain.RData", compress = TRUE)

## check whether the the adjusted input is no longer able to predict age
sc1 <- AgeAdjustmentSanityCheck(n.runs = 1000,
                                dat = dat1,
                                label.name = "combined_upper_pain", 
                                feature.names = "total_rotation", 
                                confounder.names = "age",
                                subject.id.name = "participantId",
                                neg.class.name = "FALSE", 
                                pos.class.name = "TRUE",
                                train.prop = 0.5,
                                my.seed = 12345)


## run PsA vs PsO analyses
c2 <- RunGlmCA(n.runs = 1000,
               dat = dat2,
               label.name = "PsA", 
               feature.names = "total_rotation", 
               confounder.names = "age",
               subject.id.name = "participantId",
               neg.class.name = "FALSE", 
               pos.class.name = "TRUE",
               train.prop = 0.5,
               my.seed = 12345)

apply(c2$AUROCs, 2, median)


AUROCs <- c2$AUROCs
AUPRCs <- c2$AUPRCs
original.outputs <- c2$original.outputs
adjusted.outputs <- c2$adjusted.outputs
#save(AUROCs, AUPRCs, original.outputs, adjusted.outputs, file = "psorcast_outputs_updated_PsA_vs_PsO.RData", compress = TRUE)

## check whether the the adjusted input is no longer able to predict age
sc2 <- AgeAdjustmentSanityCheck(n.runs = 1000,
                                dat = dat2,
                                label.name = "PsA", 
                                feature.names = "total_rotation", 
                                confounder.names = "age",
                                subject.id.name = "participantId",
                                neg.class.name = "FALSE", 
                                pos.class.name = "TRUE",
                                train.prop = 0.5,
                                my.seed = 12345)





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
        output_tpr_fpr = "djo_model_psa_vs_pso_tpr_fpr.tsv",
        output_iter = "djo_model_psa_vs_pso_auc_iter.tsv"),
    combined_upper_pain = list(
        output_tpr_fpr = "djo_model_combined_upper_pain_tpr_fpr.tsv",
        output_iter = "djo_model_combined_upper_pain_auc_iter.tsv")
)

PARENT_ID <- "syn26842135"






purrr::map(names(REF_LIST), function(ref){
    rdata_key = REF_LIST[[ref]]$rdata_key
    glue_label = REF_LIST[[ref]]$glue_label
    metrics_list <- fetch_rdata(REF_LIST[[ref]]$id)

    
})
