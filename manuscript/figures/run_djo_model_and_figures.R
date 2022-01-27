################################################
# This script is used for generating
# djo model weights and its corresponding figures
# for Psorcast Supplementary Figures to Synapse
# @Author: elias.chaibub.neto@sagebase.org
################################################
library(PRROC)
library(ppcor)
library(synapser)
library(ROCit)
source("manuscript/utils/fetch_id_utils.R")
source("manuscript/utils/feature_extraction_utils.R")
source("manuscript/utils/helper_utils.R")
synLogin()

FIGURES_PARENT_ID <- SYN_ID_REF$figures$parent
MODEL_PARENT_ID <- SYN_ID_REF$model_performance$parent
DJO_CURATED_FEATURES <- SYN_ID_REF$curated_features$curated_djo
GIT_URL <- get_github_url(
    git_token_path = config::get("git")$token_path,
    git_repo = config::get("git")$repo,
    script_path = "manuscript/figures/run_djo_model_and_figures.R",
    ref="branch", 
    refName='main'
)

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


## fit simple logistic regression model
FitGlm <- function(dat,
                   idx.train, 
                   idx.test, 
                   label.name, 
                   feature.names,
                   neg.class.name, 
                   pos.class.name) {
    dat <- dat[, c(label.name, feature.names)]
    dat[, label.name] <- factor(as.character(dat[, label.name]), 
                                levels = c(neg.class.name, pos.class.name)) 
    my.formula <- as.formula(paste(label.name, " ~ ", paste(feature.names, collapse = " + ")))
    fit <- glm(my.formula, data = dat[idx.train,], family = "binomial")
    pred.probs <- predict(fit, dat[idx.test, -1, drop = FALSE], type = "response")
    y.test <- dat[idx.test, 1]
    neg.scores <- pred.probs[which(y.test == neg.class.name)]
    pos.scores <- pred.probs[which(y.test == pos.class.name)]
    ## interestingly, the PRROC package defines "class 0" as positive cases 
    ## and "class 1" and negative cases
    auroc.obj <- PRROC::roc.curve(scores.class0 = pos.scores, scores.class1 = neg.scores, curve = TRUE)
    auroc <- auroc.obj$auc
    pval <- TestAUC(aucObs = auroc, ytest = y.test, 
                    negClassName = neg.class.name, posClassName = pos.class.name)
    
    ## we need to use a try() because the auprc 
    ## breaks when pred.probs are all 0.5
    auprc.obj <- NA
    auprc <- NA
    aux <- try(PRROC::pr.curve(scores.class0 = pos.scores, scores.class1 = neg.scores, curve = TRUE), silent = TRUE)
    if (!inherits(aux, "try-error")) {
        auprc.obj <- aux
        auprc <- auprc.obj$auc.integral
    }
    
    list(auroc = auroc,
         auprc = auprc, 
         pred.probs = pred.probs, 
         auroc.obj = auroc.obj,
         auprc.obj = auprc.obj,
         pval = pval)
}


## performs an analytical test for testing whether the AUROC score 
## is higher than 0.5 (i.e., if the classification performance is
## better than a random guess)
TestAUC <- function(aucObs, ytest, negClassName, posClassName) {
    GetNormApproxVarAUC <- function(ytest, negClassName, posClassName) {
        ytest <- factor(ytest)
        ylevels <- levels(ytest)
        n1 <- sum(ytest == negClassName)
        n2 <- sum(ytest == posClassName)
        n <- n1 + n2
        v <- (n + 1)/(12 * n1 * n2)
        c(v = v, n = n, nNeg = n1, nPos = n2)
    }
    v <- GetNormApproxVarAUC(ytest, negClassName, posClassName)["v"]
    
    pnorm(aucObs, 0.5, sqrt(v), lower.tail = FALSE)
}


## computes Lin's concordance correlation coefficient
LinsCCC <- function(x, y) {
    2 * cov(x, y)/(var(x) + var(y) + (mean(x) - mean(y))^2)
}


## fit a linear regression model and computes the 
## CCC score
FitLm <- function(dat,
                  idx.train, 
                  idx.test, 
                  response.name, 
                  feature.names) {
    dat <- dat[, c(response.name, feature.names)]
    my.formula <- as.formula(paste(response.name, " ~ ", paste(feature.names, collapse = " + ")))
    fit <- lm(my.formula, data = dat[idx.train,])
    y.hat <- predict(fit, dat[idx.test, -1, drop = FALSE])
    y.test <- dat[idx.test, 1]
    ccc.obs <- LinsCCC(y.test, y.hat)
    
    list(ccc.obs = ccc.obs)
}


## performs the confounding adjustment
CausalityAwareAdjustment <- function(dat, 
                                     idx.train, 
                                     idx.test, 
                                     label.name, 
                                     feature.names, 
                                     confounder.name) {
    dat.train <- dat[idx.train,]
    dat.test <- dat[idx.test,]
    n.feat <- length(feature.names)
    X.c <- matrix(NA, nrow(dat), n.feat)
    colnames(X.c) <- feature.names
    
    rhs.formula <- paste(" ~ ", label.name, " + ", paste(confounder.name, collapse = " + "), sep = "")
    
    train.model.matrix <- model.matrix(as.formula(paste("~", label.name, sep = "")), 
                                       data = dat.train)
    test.model.matrix <- model.matrix(as.formula(paste("~", paste(confounder.name, collapse = " + "), sep = "")), 
                                      data = dat.test)[, -1, drop = FALSE] ## remove the intercept column
    
    for (i in seq(n.feat)) {
        my.formula <- as.formula(paste(feature.names[i], rhs.formula, sep = ""))
        fit <- lm(my.formula, data = dat.train)
        err.train <- fit$residuals
        coeffs.1 <- fit$coefficients[c(1, 2)]
        ## get only the confounder coefficient
        coeffs.2 <- fit$coefficients[-c(1, 2)] 
        ## compute the train and test counterfactual features
        X.c.train <- as.numeric(train.model.matrix %*% coeffs.1) + err.train
        X.c.test <- dat.test[, feature.names[i]] - as.numeric(test.model.matrix %*% coeffs.2)
        aux.X.c <- rep(NA, nrow(dat))
        aux.X.c[idx.train] <- X.c.train
        aux.X.c[idx.test] <- X.c.test
        X.c[, i] <- aux.X.c
    }
    dat[, feature.names] <- X.c
    
    dat
}


## get the (partial) correlation patterns 
GetPCorPatterns <- function(Yhat, Y, A) {
    CorTests <- function(R, A, Y, labelName, confName, scoreName) {
        dat <- data.frame(Y, A, R)
        names(dat) <- c(labelName, confName, scoreName)
        cors <- matrix(NA, 5, 1)
        colnames(cors) <- "estimate"
        rownames(cors) <- c(paste("cor(", scoreName, " , ", labelName, ")", sep = ""),
                            paste("cor(", scoreName, " , ", confName, ")", sep = ""),
                            paste("cor(", labelName, " , ", confName, ")", sep = ""),
                            paste("cor(", paste(scoreName, labelName, sep = " , "), " | ", confName, ")", sep = ""),
                            paste("cor(", paste(scoreName, confName, sep = " , "), " | ", labelName, ")", sep = ""))
        pvals <- cors
        colnames(pvals) <- "pval"
        aux1 <- cor.test(dat[, scoreName], dat[, labelName], method = "spearman", exact = FALSE)
        aux2 <- cor.test(dat[, scoreName], dat[, confName], method = "spearman", exact = FALSE)
        aux3 <- cor.test(dat[, labelName], dat[, confName], method = "spearman", exact = FALSE)
        aux4 <- pcor.test(dat[, scoreName], dat[, labelName], dat[, confName], method = "spearman")
        aux5 <- pcor.test(dat[, scoreName], dat[, confName], dat[, labelName], method = "spearman")
        cors[1, 1] <- aux1$estimate
        cors[2, 1] <- aux2$estimate
        cors[3, 1] <- aux3$estimate
        cors[4, 1] <- aux4$estimate
        cors[5, 1] <- aux5$estimate
        pvals[1, 1] <- aux1$p.value
        pvals[2, 1] <- aux2$p.value
        pvals[3, 1] <- aux3$p.value
        pvals[4, 1] <- aux4$p.value
        pvals[5, 1] <- aux5$p.value
        list(cors = cors, pvals = pvals)
    }
    if (!is.numeric(Y)) {
        Y <- as.numeric(as.factor(as.character(Y))) - 1
    }
    if (!is.numeric(A)) {
        A <- as.numeric(as.factor(as.character(A))) - 1
    }
    
    aux_A <- CorTests(R = Yhat, 
                      Y = Y,
                      A = A,
                      labelName = "Y", 
                      confName = "A", 
                      scoreName = "Yhat")
    cors_A <- c(unlist(aux_A[[1]])[, 1], unlist(aux_A[[2]])[, 1])
    
    list(cors_A = cors_A)
}


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


## generates and return the adjusted feature
GrabAdjustedFeature <- function(dat,
                                n.runs,
                                index,
                                label.name, 
                                feature.names,
                                confounder.names,
                                subject.id.name,
                                neg.class.name, 
                                pos.class.name,
                                train.prop = 0.5,
                                my.seed) {
    set.seed(my.seed) 
    my.seeds <- sample(seq(1e+4, 1e+5), n.runs, replace = FALSE)
    
    set.seed(my.seeds[index]) 
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
    
    dat2
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


#####################################################
## shape the data for the analysis
#####################################################
dat0 <- read.delim(synGet(DJO_CURATED_FEATURES)$path, header = TRUE, quote = "")
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
        tibble::as_tibble())
uei_data <- fetch_results(
    metrics_list, 
    "Combined Upper Pain ({auc})", 
    "combined_upper_pain")

output_ref <- list(
    md_fpr_tpr = list(
        output_filename = "djo_model_uei_md_fpr_tpr.tsv",
        analysisType = "digital jar open",
        analysisSubtype = "uei - median iter",
        pipelineStep = "prediction",
        name = "get FPR/TPR",
        description = "retrieve median FPR/TPR across iteration for ROC-AUC curve plots"),
    auc_iter = list(
        output_filename = "djo_model_uei_auc_iter.tsv",
        analysisType = "digital jar open",
        analysisSubtype = "uei - auc iter",
        pipelineStep = "prediction",
        name = "get AUC across 1k folds",
        description = "Retrieve AUC across 1k folds to check stability"))
uei_data$md_fpr_tpr %>% 
    readr::write_tsv(output_ref$md_fpr_tpr$output_filename)
uei_data$auc_iter %>% 
    readr::write_tsv(output_ref$auc_iter$output_filename)
purrr::map(output_ref, function(content){
    file <- synapser::File(
        content$output_filename, 
        parent = MODEL_PARENT_ID,
        analysisType = content$analysisType,
        analysisSubtype = content$analysisSubtype,
        pipelineStep = content$pipelineStep)
    activity = Activity(used =DJO_CURATED_FEATURES, 
                        executed = GIT_URL,
                        name = content$name,
                        description = content$description)
    synStore(file, activity = activity)
    unlink(content$output_filename)
})



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

metrics_list <- list(
    adjusted_outputs = adjusted.outputs,
    auc_iteration = AUROCs %>%
        tibble::as_tibble())
psa_data <- fetch_results(
    metrics_list, 
    "PsA vs PsO ({auc})", "PsA")


output_ref <- list(
    md_fpr_tpr = list(
        output_filename = "djo_model_psa_vs_pso_md_fpr_tpr.tsv",
        analysisType = "digital jar open",
        analysisSubtype = "psa vs pso - median iter",
        pipelineStep = "prediction",
        name = "get FPR/TPR",
        description = "retrieve median FPR/TPR across iteration for ROC-AUC curve plots"),
    auc_iter = list(
        output_filename = "djo_model_psa_vs_pso_auc_iter.tsv",
        analysisType = "digital jar open",
        analysisSubtype = "psa vs pso - auc iter",
        pipelineStep = "prediction",
        name = "get AUC across 1k folds",
        description = "Retrieve AUC across 1k folds to check stability"))
psa_data$md_fpr_tpr %>% 
    readr::write_tsv(output_ref$md_fpr_tpr$output_filename)
psa_data$auc_iter %>% 
    readr::write_tsv(output_ref$auc_iter$output_filename)
purrr::map(output_ref, function(content){
    file <- synapser::File(
        content$output_filename, 
        parent = MODEL_PARENT_ID,
        analysisType = content$analysisType,
        analysisSubtype = content$analysisSubtype,
        pipelineStep = content$pipelineStep)
    activity = Activity(used =DJO_CURATED_FEATURES, 
                        executed = GIT_URL,
                        name = content$name,
                        description = content$description)
    synStore(file, activity = activity)
    unlink(content$output_filename)
})

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



###########################################
## generate supplement figures 
###########################################
auc.lim <- c(0, 1)
lcex <- 1.2

## Figure S1
figpath <- "Figure_S1.png"
png(figpath, width = 800, 
    height = 400, res = 100)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) + 0.1)
boxplot(c1$AUROCs[, c(1, 3)], 
        ylim = auc.lim, ylab = "AUROC",
        main = "UEI classification", 
        col = "white",
        border = c("darkblue", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(a)", at = 0, cex = lcex)
boxplot(c2$AUROCs[, c(1, 3)], ylim = auc.lim, ylab = "AUROC",
        main = "PsA-without-UEI classification", 
        col = "white",
        border = c("darkblue", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


## Figure S2
p1 <- sum(dat1$combined_upper_pain == "TRUE")/nrow(dat1)
p2 <- sum(dat2$PsA == "TRUE")/nrow(dat2)
figpath <- "Figure_S2.png"
png(figpath, width = 800, 
    height = 400, res = 100)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) + 0.1)
boxplot(c1$AUPRCs[, c(1, 3)], ylim = auc.lim, ylab = "AUPRC",
        main = "UEI classification", 
        col = "white",
        border = c("darkblue", "grey"))
abline(h = p1, col = "red")
mtext(side = 3, "(a)", at = 0, cex = lcex)
boxplot(c2$AUPRCs[, c(1, 3)], ylim = auc.lim, ylab = "AUPRC",
        main = "PsA-without-UEI", 
        col = "white",
        border = c("darkblue", "grey"))
abline(h = p2, col = "red")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


## Figure S3
figpath <- "Figure_S3.png"
png(figpath, width = 1100, 
    height = 300, res = 100)
par(mfrow = c(1, 4), mar = c(4, 3, 1.5, 1), mgp = c(2, 0.75, 0))
fit1 <- lm(dat1$age ~ dat1$total_rotation)
plot(dat1$total_rotation, dat1$age,  
     main = "age vs total-rotation (dataset 1)",
     ylab = "age",
     xlab = "total-rotation",
     ylim = c(20, 75))
abline(a = fit1$coefficients[1], b = fit1$coefficients[2], col = "red")
mtext(side = 3, "(a)", at = 50, cex = lcex)
##########
fit2 <- lm(dat2$age ~ dat2$total_rotation)
plot(dat2$total_rotation, dat2$age,  
     main = "age vs total-rotation (dataset 2)",
     ylab = "age",
     xlab = "total-rotation",
     ylim = c(20, 75))
abline(a = fit2$coefficients[1], b = fit2$coefficients[2], col = "red")
mtext(side = 3, "(b)", at = 250, cex = lcex)
##########
boxplot(dat1$age ~ dat1$combined_upper_pain,
        main = "age vs UEI",
        col = "white",
        ylab = "age",
        xlab = "UEI")
mtext(side = 3, "(c)", at = 0.3, cex = lcex)
##########
boxplot(dat2$age ~ dat2$PsA,
        main = "age vs PsA-without-UEI",
        col = "white",
        ylab = "age",
        xlab = "PsA-without-UEI")
mtext(side = 3, "(d)", at = 0.3, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


## Figure S5
lcex <- 1
figpath <- "Figure_S5.png"
png(figpath, width = 1000, 
    height = 400, res = 100)
par(mfrow = c(1, 3), 
    mar = c(5, 4, 2, 1), 
    mgp = c(2, 0.75, 0))
boxplot(ic$CCCs.age, 
        col = "white",
        ylab = "CCC", 
        border = c("darkblue", "grey"),
        main = "age prediction")
abline(h = 0, col = "red")
mtext(side = 3, "(a)", at = 0.4, cex = lcex)
boxplot(ic$AUROCs.sex, 
        col = "white",
        ylim = auc.lim, 
        ylab = "AUROC",
        main = "gender classification", 
        border = c("darkblue", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(b)", at = 0.4, cex = lcex)
boxplot(ic$AUROCs.site, 
        col = "white",
        ylim = auc.lim, 
        ylab = "AUROC",
        main = "site classification", 
        border = c("darkblue", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(c)", at = 0.4, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


## Figure S6
figpath <- "Figure_S6.png"
png(figpath, width = 1000, 
    height = 400, res = 100)
par(mfrow = c(1, 2), mar = c(4, 3, 1.5, 1), mgp = c(2, 0.75, 0))
hist(ic$AUROCs.sex.pvals[, 2], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 2.1), xlab = "p-value", main = "gender classification")
hist(ic$AUROCs.sex.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topleft", legend = c("original", "shuffled"), text.col = c("darkblue", "darkgrey"), bty = "n")
mtext(side = 3, "(a)", at = 0, cex = lcex)
####
hist(ic$AUROCs.site.pvals[, 2], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 2.1), xlab = "p-value", main = "site classification")
hist(ic$AUROCs.site.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topleft", legend = c("original", "shuffled"), text.col = c("darkblue", "darkgrey"), bty = "n")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S7
figpath <- "Figure_S7.png"
png(figpath, 
    width = 1000, 
    height = 400, 
    res = 100)
par(mfrow = c(1, 3), mar = c(4, 3, 1.5, 1), mgp = c(2, 0.75, 0))
fit <- lm(dat1$total_rotation ~ dat1$age)
plot(dat1$age, dat1$total_rotation,  
     main = "age vs total_rotation",
     ylab = "total_rotation",
     xlab = "age")
abline(a = fit$coefficients[1], b = fit$coefficients[2], col = "red")
mtext(side = 3, "(a)", at = 20, cex = lcex)
##########
boxplot(dat1$total_rotation ~ dat1$sex,
        main = "gender vs total_rotation",
        col = "white",
        ylab = "total_rotation",
        xlab = "gender")
mtext(side = 3, "(b)", at = 0.5, cex = lcex)
##########
boxplot(dat1$total_rotation ~ dat1$site,
        main = "site vs total_rotation",
        col = "white",
        ylab = "total_rotation",
        xlab = "site")
mtext(side = 3, "(c)", at = 0.5, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S12
cor.test(dat1$age, dat1$total_rotation)
cor.test(dat1$total_rotation, as.numeric(dat1$sex))
cor.test(dat1$total_rotation, as.numeric(dat1$site))
cor.lim <- c(-1, 1)

figpath <- "Figure_S12.png"
png(figpath, width = 1000, 
    height = 700, res = 100)

par(mfrow = c(1, 2), mar = c(6, 4, 3, 1))
boxplot(c1$pcors$age[, 1:5], 
        las = 2, 
        ylab = "(partial) correlations",
        col = "white",
        main = "UEI (unadjusted)", 
        ylim = cor.lim, cex = 0.5)
abline(h = 0, col = "red")
mtext(side = 3, "(a)", at = 0.5, cex = lcex)
####
aux.fig <- c2$pcors$age[, 1:5]
colnames(aux.fig) <- c("cor(R,D)", "cor(R,C)", "cor(C,D)", "cor(R,D|C)", "cor(R,C|D)")
boxplot(aux.fig, 
        las = 2,
        col = "white",
        ylab = "(partial) correlations",
        main = "PsA-without-UEI (unadjusted)", 
        ylim = cor.lim, cex = 0.5)
abline(h = 0, col = "red")
mtext(side = 3, "(b)", at = 0.5, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


## Figure S14
lcex <- 1.2
figpath <- "Figure_S14.png"
png(figpath, width = 1000, 
    height = 500, res = 100)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) + 0.1)
boxplot(c1$AUROCs, 
        ylim = auc.lim, 
        ylab = "AUROC",
        col = "white",
        main = "UEI classification", 
        border = c("darkblue", "darkgreen", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(a)", at = 0, cex = lcex)
####
boxplot(c2$AUROCs, 
        ylim = auc.lim, 
        ylab = "AUROC",
        col = "white",
        main = "PsA-without-UEI classif", 
        border = c("darkblue", "darkgreen", "grey"))
abline(h = 0.5, col = "red")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S15
nc <- 15
figpath <- "Figure_S15.png"
png(figpath, width = 1000, 
    height = 500, res = 100)
par(mfrow = c(1, 2), mar = c(4, 3, 1.5, 1), mgp = c(2, 0.75, 0))
hist(c1$AUROCs.pvals[, 3], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 16.1), xlab = "p-value", main = "UEI classification", nclass = nc)
hist(c1$AUROCs.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
hist(c1$AUROCs.pvals[, 2], probability = TRUE, col = rgb(0, 1, 0, 0.5), add = TRUE, nclass = nc)
legend("topright", legend = c("unadjusted", "adjusted", "shuffled"), text.col = c("darkblue", "darkgreen", "darkgrey"), bty = "n")
mtext(side = 3, "(a)", at = 0, cex = lcex)
####
hist(c2$AUROCs.pvals[, 3], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 16.1), xlab = "p-value", main = "PsA-without-UEI classif", nclass = nc)
hist(c2$AUROCs.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE, nclass = nc)
hist(c2$AUROCs.pvals[, 2], probability = TRUE, col = rgb(0, 1, 0, 0.5), add = TRUE, nclass = nc)
legend("topright", legend = c("unadjusted", "adjusted", "shuffled"), text.col = c("darkblue", "darkgreen", "darkgrey"), bty = "n")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S16
figpath <- "Figure_S16.png"
png(figpath, width = 1000, 
    height = 500, res = 100)
p1 <- sum(dat1$combined_upper_pain == "TRUE")/nrow(dat1)
p2 <- sum(dat2$PsA == "TRUE")/nrow(dat2)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2) + 0.1)
boxplot(c1$AUPRCs, 
        ylim = auc.lim, 
        ylab = "AUPRC",
        col = "white",
        main = "UEI classification", 
        border = c("darkblue", "darkgreen", "grey"))
abline(h = p1, col = "red")
mtext(side = 3, "(a)", at = 0, cex = lcex)
####
boxplot(c2$AUPRCs, 
        ylim = auc.lim, 
        ylab = "AUPRC",
        main = "PsA-without-UEI classif", 
        col = "white",
        border = c("darkblue", "darkgreen", "grey"))
abline(h = p2, col = "red")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S17
figpath <- "Figure_S17.png"
png(figpath, width = 1000, 
    height = 500, res = 100)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), mgp = c(2, 0.75, 0))
boxplot(sc1$CCCs.age, 
        ylab = "CCC", 
        col = "white",
        border = c("darkblue", "darkgreen", "grey"),
        main = "age prediction (dataset 1)", 
        ylim = c(-0.45, 0.45))
abline(h = 0, col = "red")
mtext(side = 3, "(a)", at = 0, cex = lcex)
boxplot(sc2$CCCs.age,
        col = "white",
        ylab = "CCC", 
        border = c("darkblue", "darkgreen", "grey"),
        main = "age prediction (dataset 2)", 
        ylim = c(-0.45, 0.45))
abline(h = 0, col = "red")
mtext(side = 3, "(b)", at = 0, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)

## Figure S18
figpath <- "Figure_S18.png"
png(figpath, width = 1000, 
    height = 500, res = 100)
par(mfrow = c(1, 2), mar = c(6, 4, 3, 1))
boxplot(c1$pcors_c$age[, 1:5], 
        color = "white",
        las = 2, ylab = "(partial) correlations",
        main = "UEI (adjusted)", 
        ylim = cor.lim, cex = 0.5)
abline(h = 0, col = "red")
mtext(side = 3, "(a)", at = 0.5, cex = lcex)
####
aux.fig <- c2$pcors_c$age[, 1:5]
colnames(aux.fig) <- c("cor(R,D)", "cor(R,C)", "cor(C,D)", "cor(R,D|C)", "cor(R,C|D)")
boxplot(aux.fig,
        color = "white",
        las = 2, 
        ylab = "(partial) correlations",
        main = "PsA-without-UEI (adjusted)", 
        ylim = cor.lim, cex = 0.5)
abline(h = 0, col = "red")
mtext(side = 3, "(b)", at = 0.5, cex = lcex)
dev.off()
file <- synapser::File(figpath, 
                       parent = FIGURES_PARENT_ID,
                       analysisType = "digital jar open",
                       pipelineStep = "figures",
                       task = "digital jar open")
activity <- Activity(used = DJO_CURATED_FEATURES,
                     executed = GIT_URL)
synapser::synStore(file, activity = activity)
unlink(figpath)


###########################################
## grab adjusted inputs 
###########################################

adat1 <- GrabAdjustedFeature(dat = dat1,
                             n.runs = 1000,
                             index = 41,
                             label.name = "combined_upper_pain", 
                             feature.names = "total_rotation", 
                             confounder.names = "age",
                             subject.id.name = "participantId",
                             neg.class.name = "FALSE", 
                             pos.class.name = "TRUE",
                             train.prop = 0.5,
                             my.seed = 12345)

adat2 <- GrabAdjustedFeature(dat = dat2,
                             n.runs = 1000,
                             index = 100,
                             label.name = "PsA", 
                             feature.names = "total_rotation", 
                             confounder.names = "age",
                             subject.id.name = "participantId",
                             neg.class.name = "FALSE", 
                             pos.class.name = "TRUE",
                             train.prop = 0.5,
                             my.seed = 12345)

#save(adat1, file = "adjusted_feature_combined_upper_pain_index_41.RData", compress = TRUE)
#save(adat2, file = "adjusted_feature_PsA_vs_PsO_index_100.RData", compress = TRUE)


################################################
## final checks
################################################

## remove repeated measurements
dat3 <- dat1[!duplicated(dat1$participantId),]

ic3 <- InformalChecks(n.runs = 1000,
                      dat = dat3,
                      feature.names = "total_rotation",
                      subject.id.name = "participantId",
                      train.prop = 0.5,
                      my.seed = 12345)

## results do not change after we remove the repreated measurements to 
## make sure the analitycal test is valid
par(mfrow = c(1, 2), mar = c(4, 3, 1.5, 1), mgp = c(2, 0.75, 0))
hist(ic3$AUROCs.sex.pvals[, 2], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 2.1), xlab = "p-value", main = "gender classification")
hist(ic3$AUROCs.sex.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topleft", legend = c("original", "shuffled"), text.col = c("darkblue", "darkgrey"), bty = "n")
mtext(side = 3, "(a)", at = 0, cex = lcex)
####
hist(ic3$AUROCs.site.pvals[, 2], probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), 
     ylim = c(0, 2.1), xlab = "p-value", main = "site classification")
hist(ic3$AUROCs.site.pvals[, 1], probability = TRUE, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topleft", legend = c("original", "shuffled"), text.col = c("darkblue", "darkgrey"), bty = "n")
mtext(side = 3, "(b)", at = 0, cex = lcex)



###############################################
## computing AUROC confidence intervals
###############################################

library(pROC)

GetAdjustedAurocCI <- function(dat,
                               n.runs,
                               index,
                               label.name, 
                               feature.names,
                               confounder.names,
                               subject.id.name,
                               neg.class.name, 
                               pos.class.name,
                               train.prop = 0.5,
                               my.seed,
                               ci.method = "delong",
                               conf.level = 0.95) {
    set.seed(my.seed) 
    my.seeds <- sample(seq(1e+4, 1e+5), n.runs, replace = FALSE)
    
    set.seed(my.seeds[index]) 
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
    dat2 <- dat2[, c(label.name, feature.names)]
    dat2[, label.name] <- factor(as.character(dat2[, label.name]), 
                                 levels = c(neg.class.name, pos.class.name)) 
    
    my.formula <- as.formula(paste(label.name, " ~ ", paste(feature.names, collapse = " + ")))
    
    fit2 <- glm(my.formula, data = dat2[idx.train,], family = "binomial")
    pred.probs2 <- predict(fit2, dat2[idx.test, -1, drop = FALSE], type = "response")
    y.test2 <- dat2[idx.test, 1]
    rocObj2 <- roc(y.test2, pred.probs2, direction = "<", 
                   levels = c(neg.class.name, pos.class.name))    
    auroc.obs.adj <- pROC::auc(rocObj2)[1]
    auroc.ci.adj <- pROC::ci.auc(rocObj2, conf.level = conf.level, method = ci.method)
    
    list(auroc.obs.adj = auroc.obs.adj, 
         auroc.ci.adj = auroc.ci.adj)
}



## get the index of the data split closest to the median
idx.1 <- which.min(abs(c1$AUROCs[, "adjusted"] - median(c1$AUROCs[, "adjusted"])))
idx.2 <- which.min(abs(c2$AUROCs[, "adjusted"] - median(c2$AUROCs[, "adjusted"])))

idx.1
idx.2


aux1b <- GetAdjustedAurocCI(dat = dat1,
                            n.runs = 1000,
                            index = idx.1,
                            label.name = "combined_upper_pain", 
                            feature.names = "total_rotation", 
                            confounder.names = "age",
                            subject.id.name = "participantId",
                            neg.class.name = "FALSE", 
                            pos.class.name = "TRUE",
                            train.prop = 0.5,
                            my.seed = 12345,
                            ci.method = "bootstrap",
                            conf.level = 0.95)

aux2b <- GetAdjustedAurocCI(dat = dat2,
                            n.runs = 1000,
                            index = idx.2,
                            label.name = "PsA", 
                            feature.names = "total_rotation", 
                            confounder.names = "age",
                            subject.id.name = "participantId",
                            neg.class.name = "FALSE", 
                            pos.class.name = "TRUE",
                            train.prop = 0.5,
                            my.seed = 12345,
                            ci.method = "bootstrap",
                            conf.level = 0.95)

aux1b
aux2b

