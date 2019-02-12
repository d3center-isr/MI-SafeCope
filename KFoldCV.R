setwd("J:\\MI-SafeCope")
library(pROC)
library(cvTools)
set.seed(8759283)

# Models ----------------------------------------------------------------------
# Best Simple Model
my_fo <- m1_binaryoutcome ~ mean.Self_Efficacy.2
# Best Complex Model
my_fo <- m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+mean.Self_Efficacy.2+var.Self_Efficacy.2+mean.Duration_Combined.2+var.Duration_Combined.2

# Other complex models
my_fo <- m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+mean.Hopelessness.2+var.Hopelessness.2+mean.Self_Efficacy.2+var.Self_Efficacy.2
my_fo <- m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+mean.Self_Efficacy.2+var.Self_Efficacy.2+mean.Duration_Combined.2+var.Duration_Combined.2
my_fo <- m1_binaryoutcome ~ mean.Self_Efficacy.2+var.Self_Efficacy.2+mean.Duration_Combined.2+var.Duration_Combined.2
my_fo <- m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+mean.Self_Efficacy.2+var.Self_Efficacy.2
my_fo <- m1_binaryoutcome ~ mean.Miserable.2 + mean.Hopelessness.2+mean.Self_Efficacy.2+mean.Duration_Combined.2
my_fo <- m1_binaryoutcome ~ mean.Miserable.2 + mean.Hopelessness.2+mean.Self_Efficacy.2
my_fo <- m1_binaryoutcome ~ mean.Hopelessness.2+mean.Self_Efficacy.2+mean.Duration_Combined.2

# Data ------------------------------------------------------------------------
m1.data <- read.csv("m1.data.csv", header = TRUE)
m1.data <- m1.data[!is.na(m1.data$m1_binaryoutcome),]
m1.data.ones <- m1.data[m1.data$m1_binaryoutcome==1,]
row.names(m1.data.ones) <- 1:nrow(m1.data.ones)
m1.data.zeros <- m1.data[m1.data$m1_binaryoutcome==0,]
row.names(m1.data.zeros) <- 1:nrow(m1.data.zeros)

# Functions here --------------------------------------------------------------
GetValidationDat <- function(this.fold, df = m1.data){
  train.data <- df[df$fold != this.fold,]
  holdout.data <- df[df$fold == this.fold,]
  results.list <- list(train.data = train.data, holdout.data = holdout.data)
  return(results.list)
}

GetHoldoutPredProb <- function(dat.list, fo = my_fo, outcome="m1_binaryoutcome"){
  train.data <- dat.list$train.data
  holdout.data <- dat.list$holdout.data
  fit <- glm(fo, data = train.data, family = "binomial")
  this.participant.pred.prob <- predict.glm(fit, newdata = holdout.data, type = "response")
  this.participant.dat <- data.frame(ID = holdout.data$ID,
                                     actual.outcome = holdout.data[,outcome],
                                     this.participant.pred.prob = this.participant.pred.prob)
  
  holdout.data <- dat.list$holdout.data
  this.participant.dat$fold.held.out <- holdout.data$fold[1]
  
  return(this.participant.dat)
}

# Get Data for k-fold cross validation ----------------------------------------
ones.folds <- cvFolds(nrow(m1.data.ones), K = nrow(m1.data.ones), type = "random")
zeros.folds <- cvFolds(nrow(m1.data.zeros), K = nrow(m1.data.ones), type = "random")

dat.ones.folds <- data.frame(fold = ones.folds$which, index = ones.folds$subsets)
dat.zeros.folds <- data.frame(fold = zeros.folds$which, index = zeros.folds$subsets)
dat.ones.folds <- dat.ones.folds[order(dat.ones.folds$index) ,]
dat.zeros.folds <- dat.zeros.folds[order(dat.zeros.folds$index) ,]
m1.data.ones <- cbind(m1.data.ones, dat.ones.folds)
m1.data.zeros <- cbind(m1.data.zeros, dat.zeros.folds)
m1.data <- rbind(m1.data.ones, m1.data.zeros)

all.dat <- lapply(1:nrow(m1.data.ones), GetValidationDat)

# Get Predicted Probabilities -------------------------------------------------
all.predicted.probs.dat <- lapply(all.dat, GetHoldoutPredProb)
all.holdout.auc <- lapply(all.predicted.probs.dat, function(this.holdout){
  roc.obj <- roc(this.holdout$actual.outcome, this.holdout$this.participant.pred.prob)
  return(roc.obj$auc)
})

# Calculate AUC on hold out sets and then take the average of AUC across all --
all.holdout.auc <- do.call(rbind, all.holdout.auc)
mean(all.holdout.auc)
