## dependencies
library(PRROC)
library(ppcor)
library(synapser)
library(tidyverse)
library(data.table)

synapser::synLogin()

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
    auprc.obj <- PRROC::pr.curve(scores.class0 = pos.scores, scores.class1 = neg.scores, curve = TRUE)
    auroc <- auroc.obj$auc
    auprc <- auprc.obj$auc.integral
    pval <- TestAUC(aucObs = auroc, ytest = y.test, 
                    negClassName = neg.class.name, posClassName = pos.class.name)
    
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
        cors <- matrix(NA, 6, 1)
        colnames(cors) <- "estimate"
        rownames(cors) <- c(paste("cor(", scoreName, " , ", labelName, ")", sep = ""),
                            paste("cor(", scoreName, " , ", confName, ")", sep = ""),
                            paste("cor(", labelName, " , ", confName, ")", sep = ""),
                            paste("cor(", paste(scoreName, labelName, sep = " , "), " | ", confName, ")", sep = ""),
                            paste("cor(", paste(scoreName, confName, sep = " , "), " | ", labelName, ")", sep = ""),
                            paste("cor(", paste(labelName, confName, sep = " , "), " | ", scoreName, ")", sep = ""))
        pvals <- cors
        colnames(pvals) <- "pval"
        aux1 <- cor.test(dat[, scoreName], dat[, labelName], method = "spearman", exact = FALSE)
        aux2 <- cor.test(dat[, scoreName], dat[, confName], method = "spearman", exact = FALSE)
        aux3 <- cor.test(dat[, labelName], dat[, confName], method = "spearman", exact = FALSE)
        aux4 <- pcor.test(dat[, scoreName], dat[, labelName], dat[, confName], method = "spearman")
        aux5 <- pcor.test(dat[, scoreName], dat[, confName], dat[, labelName], method = "spearman")
        aux6 <- pcor.test(dat[, labelName], dat[, confName], dat[, scoreName], method = "spearman")
        cors[1, 1] <- aux1$estimate
        cors[2, 1] <- aux2$estimate
        cors[3, 1] <- aux3$estimate
        cors[4, 1] <- aux4$estimate
        cors[5, 1] <- aux5$estimate
        cors[6, 1] <- aux6$estimate
        pvals[1, 1] <- aux1$p.value
        pvals[2, 1] <- aux2$p.value
        pvals[3, 1] <- aux3$p.value
        pvals[4, 1] <- aux4$p.value
        pvals[5, 1] <- aux5$p.value
        pvals[6, 1] <- aux6$p.value
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

## runs the experiments on the original, adjusted, and shuffled
## data for the label classification, gender classification, 
## and age prediction
RunGlmCA <- function(n.runs,
                     dat,
                     label.name, 
                     feature.names,
                     confounder.names,
                     subject.id.name,
                     neg.class.name, 
                     pos.class.name,
                     train.prop = 0.5) {
    original.outputs <- vector(mode = "list", length = n.runs)
    adjusted.outputs <- vector(mode = "list", length = n.runs)
    AUROCs <- matrix(NA, n.runs, 3)
    colnames(AUROCs) <- c("original", "adjusted", "shuffled")
    CCCs.age <- AUROCs
    AUROCs.sex <- AUROCs
    AUROCs.pvals <- AUROCs
    AUROCs.sex.pvals <- AUROCs
    AUPRCs <- AUROCs
    AUPRCs.sex <- AUROCs
    
    n.confounders <- length(confounder.names)
    pcors <- vector(mode = "list", length(n.confounders))
    for (i in seq(n.confounders)) {
        aux <- matrix(NA, n.runs, 12)
        colnames(aux) <- c("cor(Yhat,Y)", "cor(Yhat,A)", "cor(A,Y)", 
                           "cor(Yhat,Y|A)", "cor(Yhat,A|Y)", "cor(A,Y|Yhat)",
                           "pval(Yhat,Y)", "pval(Yhat,A)", "pval(A,Y)", 
                           "pval(Yhat,Y|A)", "pval(Yhat,A|Y)", "pval(A,Y|Yhat)")
        pcors[[i]] <- aux
    }
    names(pcors) <- confounder.names
    pcors_c <- pcors
    
    dat <- dat[, c(subject.id.name, label.name, feature.names, "age", "sex", "visit_num")]
    dat <- na.omit(dat)
    n <- nrow(dat)
    for (i in seq(n.runs)) {
        cat(i, "\n")
        
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
            
            pcors[[j]][i, 1:6] <- pcors1$cors_A[1:6]
            pcors[[j]][i, 7:12] <- pcors1$cors_A[7:12]
        }
        tmp <- dat[idx.test, c("participantId", "visit_num", label.name)]
        tmp$pred.prob <- aux1$pred.probs
        original.outputs[[i]] <- list(test.data = tmp, 
                                      auroc.obj = aux1$auroc.obj,
                                      auprc.obj = aux1$auprc.obj)
        
        ## shuffled data
        pdat <- dat
        pdat[, label.name] <- dat[sample(nrow(dat)), label.name]
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
            
            pcors_c[[j]][i, 1:6] <- pcors2$cors_A[1:6]
            pcors_c[[j]][i, 7:12] <- pcors2$cors_A[7:12]
        }
        tmp2 <- dat2[idx.test, c("participantId", "visit_num", label.name)]
        tmp2$pred.prob <- aux2$pred.probs ## replace the predicted prob
        adjusted.outputs[[i]] <- list(test.data = tmp, 
                                      auroc.obj = aux2$auroc.obj,
                                      auprc.obj = aux2$auprc.obj)
        
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
        
        ## adjusted data
        aux2 <- FitGlm(dat2,
                       idx.train, 
                       idx.test, 
                       label.name = "sex", 
                       feature.names = feature.names,
                       neg.class.name = "Female", 
                       pos.class.name = "Male")
        AUROCs.sex[i, "adjusted"] <- aux2$auroc 
        AUROCs.sex.pvals[i, "adjusted"] <- aux2$pval
        AUPRCs.sex[i, "adjusted"] <- aux2$auprc
        
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
        
        ## adjusted data
        aux2 <- FitLm(dat2,
                      idx.train, 
                      idx.test, 
                      response.name = "age", 
                      feature.names = feature.names)
        CCCs.age[i, "adjusted"] <- aux2$ccc.obs   
        
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
    
    list(AUROCs = AUROCs,
         AUPRCs = AUPRCs,
         AUROCs.pvals = AUROCs.pvals,
         CCCs.age = CCCs.age,
         AUROCs.sex = AUROCs.sex,
         AUPRCs.sex = AUPRCs.sex,
         AUROCs.sex.pvals = AUROCs.sex.pvals,
         pcors = pcors,
         pcors_c = pcors_c,
         original.outputs = original.outputs,
         adjusted.outputs = adjusted.outputs)
}




###############################################
###############################################
###############################################
feat1 <- read.delim(synGet(DJO_SYN_ID)$path, header = TRUE, quote = "")
feat.names.1 <- feat1 %>%
    dplyr::select(matches("djo|rotation")) %>%
    names(.)

plot_list <- list()
plot_list$age <- list()
plot_list$sex <- list()

plot_list$age$total_rotation <- feat1 %>%
    ggplot(aes(x = total_rotation,
               y = age)) +
    geom_smooth(method = "lm",
                se = F) +
    geom_point(alpha = 0.5) +
    stat_cor(method = "pearson") +
    theme_minimal()


plot_list$age$combined_upper_pain <- feat1 %>%
    ggplot(aes(x = combined_upper_pain,
               y = age)) +
    geom_boxplot() +
    geom_point(alpha = 0.5) +
    stat_compare_means() +
    theme_minimal()

plot_list$age$upper_body_pain <- feat1 %>%
    ggplot(aes(x = upper_body_pain,
               y = age)) +
    geom_boxplot() +
    geom_point(alpha = 0.5) +
    stat_compare_means() +
    theme_minimal()

plot_list$age$plot <- plot_list$age %>% 
    patchwork::wrap_plots()




## gender associations
plot_list$sex$total_rotation <- feat1 %>%
    ggplot(aes(y = total_rotation,
               x = sex)) +
    geom_boxplot() +
    geom_point(alpha = 0.5) +
    stat_compare_means() +
    theme_minimal()

plot_list$sex$combined_upper_pain <- feat1 %>%
    ggplot() +
    geom_mosaic(aes(
        x = product(sex),
        fill = combined_upper_pain)) +
    theme_mosaic() +
    theme(legend.position = "none")

plot_list$sex$upper_body_pain <- feat1 %>%
    ggplot() +
    geom_mosaic(aes(
        x = product(sex),
        fill = upper_body_pain)) +
    theme_mosaic() +
    theme(legend.position = "none")

plot_list$sex$plot <- plot_list$sex %>% 
    patchwork::wrap_plots()

cor.test(as.numeric(feat1$sex)-1, feat1$total_rotation)
chisq.test(feat1$sex, feat1$combined_upper_pain)
chisq.test(feat1$sex, feat1$upper_body_pain)


#####################################################
## combined_upper_pain
#####################################################

set.seed(123)
a1 <- RunGlmCA(n.runs = 1000,
               dat = feat1,
               label.name = "combined_upper_pain", 
               feature.names = "total_rotation", 
               confounder.names = c("age"),
               subject.id.name = "participantId",
               neg.class.name = "FALSE", 
               pos.class.name = "TRUE",
               train.prop = 0.5)


benchmark_bplot <- function(data, factor_order){
    data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(everything()) %>%
        dplyr::mutate(name = factor(name, 
                                    levels = factor_order)) %>%
        ggplot(aes(x = name, y = value)) +
        geom_boxplot() +
        ylim(0,1)
}


#save(AUROCs, AUPRCs, original.outputs, adjusted.outputs, file = "psorcast_AUROC_outputs_combined_upper_pain_new_PRROC.RData", compress = TRUE)
pred_plot <- list()
pred_plot$combined$plot_auc_bmarker <- a1$AUROCs %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "DjO Features",
         x = "",
         y = "AU-ROC") +
    theme_minimal() 


pred_plot$combined$plot_ccc <- a1$CCCs.age %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0,
               linetype = "twodash",
               color = "red") +
    labs(title = "Age",
         x = "",
         y = "CCC") +
    theme_minimal()


pred_plot$combined$plot_auc_sex <- a1$AUROCs.sex%>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "AU-ROC") +
    theme_minimal()


pred_plot$combined$plot_aupr <- a1$AUPRCs %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Djo Features",
         x = "",
         y = "AU-PR") +
    theme_minimal()


pred_plot$combined$plot_aupr_sex <- a1$AUPRCs.sex %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "AU-PR") +
    theme_minimal()

pred_plot$combined$plot_auroc_pvals <- a1$AUROCs.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Djo Features",
         x = "",
         y = "P-Values") +
    theme_minimal()


pred_plot$combined$plot_auroc_sex_pvals <- a1$AUROCs.sex.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "P-Values") +
    theme_minimal()

pred_plot$combined$plot_auroc_sex_pvals <- a1$AUROCs.sex.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "P-Values") +
    theme_minimal()


pred_plot$combined$plot_partial_orig <- a1$pcors$age[, 1:6] %>% 
    benchmark_bplot(factor_order = names(tibble::as_tibble(.))) +
    geom_hline(yintercept = 0, color = "red") +
    labs(y = "(Partial) Correlations",
         x = "",
         title = "Original") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

pred_plot$combined$plot_partial_adj <- a1$pcors_c$age[, 1:6] %>% 
    benchmark_bplot(factor_order = names(tibble::as_tibble(.))) +
    geom_hline(yintercept = 0, color = "red") +
    labs(y = "(Partial) Correlations",
         x = "",
         title = "Age Adjusted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))


plot_row1 <- patchwork::wrap_plots(
    pred_plot$combined$plot_auc_bmarker,
    pred_plot$combined$plot_ccc,
    pred_plot$combined$plot_auc_sex)

plot_row2 <- patchwork::wrap_plots(
    pred_plot$combined$plot_partial_orig,
    pred_plot$combined$plot_partial_adj)

plot_list$pred$combined <- patchwork::wrap_plots(
    plot_row1,
    plot_row2,
    nrow = 2) +
    plot_annotation(title = "Combined Upper Pain",
                    subtitle = "Upper Joint Pain + Enthesitis")
    







#####################################################
## upper_body_pain
#####################################################
set.seed(123)
a2 <- RunGlmCA(n.runs = 1000,
               dat = feat1,
               label.name = "upper_body_pain", 
               feature.names = "total_rotation", 
               confounder.names = c("age"),
               subject.id.name = "participantId",
               neg.class.name = "FALSE", 
               pos.class.name = "TRUE",
               train.prop = 0.5)


#save(AUROCs, AUPRCs, original.outputs, adjusted.outputs, file = "psorcast_AUROC_outputs_combined_upper_pain_new_PRROC.RData", compress = TRUE)
pred_plot <- list()
pred_plot$combined$plot_auc_bmarker <- a2$AUROCs %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "DjO Features",
         x = "",
         y = "AU-ROC") +
    theme_minimal() 


pred_plot$combined$plot_ccc <- a2$CCCs.age %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0,
               linetype = "twodash",
               color = "red") +
    labs(title = "Age",
         x = "",
         y = "CCC") +
    theme_minimal()


pred_plot$combined$plot_auc_sex <- a2$AUROCs.sex%>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "AU-ROC") +
    theme_minimal()


pred_plot$combined$plot_aupr <- a2$AUPRCs %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Djo Features",
         x = "",
         y = "AU-PR") +
    theme_minimal()


pred_plot$combined$plot_aupr_sex <- a2$AUPRCs.sex %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "AU-PR") +
    theme_minimal()

pred_plot$combined$plot_auroc_pvals <- a2$AUROCs.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Djo Features",
         x = "",
         y = "P-Values") +
    theme_minimal()


pred_plot$combined$plot_auroc_sex_pvals <- a2$AUROCs.sex.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "P-Values") +
    theme_minimal()

pred_plot$combined$plot_auroc_sex_pvals <- a2$AUROCs.sex.pvals %>%
    benchmark_bplot(factor_order = c("original",
                                     "adjusted",
                                     "shuffled")) +
    geom_hline(yintercept = 0.5,
               linetype = "twodash",
               color = "red") +
    labs(title = "Sex",
         x = "",
         y = "P-Values") +
    theme_minimal()


pred_plot$combined$plot_partial_orig <- a2$pcors$age[, 1:6] %>% 
    benchmark_bplot(factor_order = names(tibble::as_tibble(.))) +
    geom_hline(yintercept = 0.05, color = "red") +
    labs(y = "(Partial) Correlations",
         x = "",
         title = "Original") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

pred_plot$combined$plot_partial_adj <- a2$pcors_c$age[, 1:6] %>% 
    benchmark_bplot(factor_order = names(tibble::as_tibble(.))) +
    geom_hline(yintercept = 0.05, color = "red") +
    labs(y = "(Partial) Correlations",
         x = "",
         title = "Age Adjusted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))


plot_row1 <- patchwork::wrap_plots(
    pred_plot$combined$plot_auc_bmarker,
    pred_plot$combined$plot_ccc,
    pred_plot$combined$plot_auc_sex)

plot_row2 <- patchwork::wrap_plots(
    pred_plot$combined$plot_partial_orig,
    pred_plot$combined$plot_partial_adj)

plot_list$pred$upper_body_pain <- patchwork::wrap_plots(
    plot_row1,
    plot_row2,
    nrow = 2) +
    plot_annotation(title = " Upper Body Pain",
                    subtitle = "Upper Joint Pain Only")