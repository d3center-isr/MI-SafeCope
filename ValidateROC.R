setwd("J:\\MI-SafeCope")
library(pROC)

# Models ----------------------------------------------------------------------
# Best Simple Model
my_fo <- m1_binaryoutcome ~ mean.Self_Efficacy.2
# Best Complex Model
my_fo <- m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+mean.Self_Efficacy.2+var.Self_Efficacy.2+mean.Duration_Combined.2+var.Duration_Combined.2

# Other simple models
my_fo <- m1_binaryoutcome ~ mean.Miserable.2
my_fo <- m1_binaryoutcome ~ mean.Hopelessness.2
my_fo <- m1_binaryoutcome ~ mean.Burdensomeness.2
my_fo <- m1_binaryoutcome ~ mean.Connectedness.2
my_fo <- m1_binaryoutcome ~ mean.Self_Efficacy.2
my_fo <- m1_binaryoutcome ~ mean.Duration_Combined.2

# Other simple models
my_fo <- m1_binaryoutcome ~ mean.Miserable.2 + var.Miserable.2
my_fo <- m1_binaryoutcome ~ mean.Hopelessness.2 + var.Hopelessness.2
my_fo <- m1_binaryoutcome ~ mean.Burdensomeness.2 + var.Burdensomeness.2
my_fo <- m1_binaryoutcome ~ mean.Connectedness.2 + var.Connectedness.2
my_fo <- m1_binaryoutcome ~ mean.Self_Efficacy.2 + var.Self_Efficacy.2
my_fo <- m1_binaryoutcome ~ mean.Duration_Combined.2 + var.Duration_Combined.2

# Get Data -------------------------------------------------
m1.data <- read.csv("m1.data.csv", header = TRUE)
m1.data <- m1.data[!is.na(m1.data$m1_binaryoutcome),]

GetValidationDat <- function(this.ID, df = m1.data){
  loo.data <- df[df$ID != this.ID,]
  holdout.data <- df[df$ID == this.ID,]
  results.list <- list(loo.data = loo.data, holdout.data = holdout.data)
  return(results.list)
}

ids <- as.list(m1.data$ID)
all.dat <- lapply(ids, GetValidationDat)

# Get Predicted Probabilities -------------------------------------------------
GetHoldoutPredProb <- function(dat.list, fo = my_fo, outcome="m1_binaryoutcome"){
  loo.data <- dat.list$loo.data
  holdout.data <- dat.list$holdout.data
  fit <- glm(fo, data = loo.data, family = "binomial")
  this.participant.pred.prob <- predict.glm(fit, newdata = holdout.data, type = "response")
  this.participant.dat <- data.frame(ID = holdout.data$ID,
                                     actual.outcome = holdout.data[,outcome],
                                     this.participant.pred.prob = this.participant.pred.prob)
  
  holdout.data <- dat.list$holdout.data
  this.participant.dat$participant.held.out <- holdout.data$ID[1]
  
  return(this.participant.dat)
}

all.results.list <- lapply(all.dat, GetHoldoutPredProb)
all.results.mat <- do.call(rbind, all.results.list)
all.results.mat$this.participant.pred.prob <- round(all.results.mat$this.participant.pred.prob, digits=3)

# Get Threshold ---------------------------------------------------------------
GetLOOPredProb <- function(dat.list, fo = my_fo, outcome="m1_binaryoutcome"){
  loo.data <- dat.list$loo.data
  fit <- glm(fo, data = loo.data, family = "binomial")
  this.participant.pred.prob <- predict.glm(fit, newdata = loo.data, type = "response")
  this.participant.dat <- data.frame(ID = loo.data$ID,
                                     actual.outcome = loo.data[,outcome],
                                     this.participant.pred.prob = this.participant.pred.prob)
  this.participant.dat$this.participant.pred.prob <- round(this.participant.dat$this.participant.pred.prob, digits=3)
  
  holdout.data <- dat.list$holdout.data
  this.participant.dat$participant.held.out <- holdout.data$ID[1]
  
  # Use pROC package
  optimality.criterion <- "closest.topleft"
  relative.cost.assigned <- 1  
  prevalence.month1 <- 0.16
  roc.obj <- roc(this.participant.dat$actual.outcome, this.participant.dat$this.participant.pred.prob)
  roc.coords <- coords(roc = roc.obj, x = "best", 
                       best.method = optimality.criterion,
                       best.weights = c(relative.cost.assigned , prevalence.month1))
  this.participant.dat$optimal.threshold <- roc.coords["threshold"]
  
  return(this.participant.dat)
}

all.results.list <- lapply(all.dat, GetLOOPredProb)
all.results.mat <- do.call(rbind, all.results.list)
loo.threshold <- by(all.results.mat, all.results.mat$participant.held.out, function(df){
  return(df$optimal.threshold[1])
})
loo.threshold <- list(loo.threshold)
loo.threshold <- do.call(cbind, loo.threshold)
mean.loo.threshold <- mean(loo.threshold)
print(mean.loo.threshold)

# Calculate Misclassification Rate --------------------------------------------
all.results.mat$threshold <- mean.loo.threshold
all.results.mat$predicted.outcome <- 1*(all.results.mat$this.participant.pred.prob > all.results.mat$threshold)
all.results.mat$error <- 1*(all.results.mat$actual.outcome != all.results.mat$predicted.outcome)
mean(all.results.mat$error)
