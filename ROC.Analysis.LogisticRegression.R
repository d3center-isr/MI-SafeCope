setwd("J:\\MI-SafeCope")
library(dplyr)# Package for data manipulation
library(pROC) # Package for obtaining ROC curves, AUCs, and statistical tests involving ROC curves
# pROC is the only R package that enables statistical tests of difference in ROC curves
# and is useful for determining whether AUC's from two different ROC curves are statistically 
# different from each other

#***********************************************************************************************************#
# Check extent of missing data
#***********************************************************************************************************#
# The file below is created by CreateDataForAnalysis.R
dat <- read.csv("LongDataForAnalysis.csv", na.strings = c(NA))

tmppltdat <- dat %>% group_by(Day) %>%
  mutate(miss.Miserable=is.na(Miserable), miss.Hopelessness=is.na(Hopelessness), 
         miss.Burdensomeness=is.na(Burdensomeness), miss.Connectedness=is.na(Connectedness),
         miss.Self_Efficacy = is.na(Self_Efficacy), miss.Duration_Combined=is.na(Duration_Combined)) %>%
  mutate(totalmiss = miss.Miserable + miss.Hopelessness + miss.Burdensomeness + miss.Connectedness + miss.Self_Efficacy + miss.Duration_Combined) %>%
  mutate(indicator.totalmiss = if_else(totalmiss > 0, 1, 0)) %>%
  select(Day, ID, indicator.totalmiss) %>% summarise(NumberOfParticipantObservationsMissing = sum(indicator.totalmiss)) %>% as.matrix(.)

plot(tmppltdat, type = "o", ylim= c(0,36), xaxt="n", main = "",
     xlab = "No. of Days Post-Discharge", ylab = "No. of Participants")
axis(side=1, at = c(1,7,14,21,28))

#***********************************************************************************************************#
# Create datasets for analyses
#***********************************************************************************************************#

# Our features consist of those constructed cumulatively by week 1 and week 2 of the study
# which are saved in two csv files. This section creates m1.data and m3.data which are wide datasets 
# for the 34 participants with cumulative weeks 1 and 2 features. The outcome used at m1.data is 
# m1_binaryoutcome, a binary variable that is equal to 1 if a participant experienced any suicide-related
# incident (i.e. specifically any of the following occurring: any emergency department visit, 
# any hospitalization, any suicide behavior) from day 15 to month 1 and equal to 0 otherwise. 
# The outcome used at m3.data is m3cum_binaryoutcome, a binary variable that is equal to 1 if a 
# participant experiences any suicide-related incident from day 15 to month 3 and 
# equal to 0 otherwise. In all, m1.data and m3.data have 112 columns each. 

cum.week1.dat <- read.csv("Week1CumulativeData.csv", header=TRUE, na.strings = c("NA"))
cum.week2.dat <- read.csv("Week2CumulativeData.csv", header=TRUE, na.strings = c("NA"))

baseline.and.outcome.vars <- cum.week1.dat %>% 
  select(ID, Female, Intervention, B_multipleattempt, B_attemptyesno, 
         m1_binaryoutcome, m3cum_binaryoutcome) %>% unique(.)

cum.week1.dat <- cum.week1.dat %>% select(-Female, -Intervention, -B_multipleattempt, -B_attemptyesno,
                                          -fm1_suicidalbeh, -m1_rehospitalization,
                                          -m1_returnEDvisit, -m1_binaryoutcome,
                                          -outcome_returnEDvisit, -outcome_attempt,
                                          -outcome_behavior, -outcome_rehospitalization,
                                          -m3cum_binaryoutcome) 

names(cum.week1.dat) <- paste0(names(cum.week1.dat),".1")  

cum.week2.dat <- cum.week2.dat %>% select(-Female, -Intervention, -B_multipleattempt, -B_attemptyesno,
                                          -fm1_suicidalbeh, -m1_rehospitalization,
                                          -m1_returnEDvisit, -m1_binaryoutcome,
                                          -outcome_returnEDvisit, -outcome_attempt,
                                          -outcome_behavior, -outcome_rehospitalization,
                                          -m3cum_binaryoutcome) 

names(cum.week2.dat) <- paste0(names(cum.week2.dat),".2")  


all.data <- inner_join(cum.week1.dat,cum.week2.dat,by = c("ID.1" = "ID.2"))
colnames(all.data)[colnames(all.data) == "ID.1"] <- "ID"
all.data <- left_join(all.data, baseline.and.outcome.vars, by = c("ID"))

all.data$m1_binaryoutcome <- as.factor(all.data$m1_binaryoutcome)
all.data$m3cum_binaryoutcome <- as.factor(all.data$m3cum_binaryoutcome)
all.data$B_multipleattempt <- as.factor(all.data$B_multipleattempt)
all.data$B_attemptyesno <- as.factor(all.data$B_attemptyesno)
all.data$Female <- as.factor(all.data$Female)
all.data$Intervention <- as.factor(all.data$Intervention)

m1.data <- subset(all.data,select = -m3cum_binaryoutcome)
m3.data <- subset(all.data,select = -m1_binaryoutcome)

#***********************************************************************************************************#
# Settings to compute the optimal probability cut-off for logistic regression classifier
#***********************************************************************************************************#

# Two choices for optimality criteria exist in the pROC package: c("youden","closest.topleft")
# A higher cost assigned to relative.cost.assigned penalizes false negatives more than false positives
# when relative.cost.assigned=1, false negatives and false positives are not penalized unequally
# prevalence.month1 is the proportion of participants for whom m1_binaryoutcome=1
# prevalence.month3 is the proportion of participants for whom m3cum_binaryoutcome=1
optimality.criterion <- "closest.topleft"
relative.cost.assigned <- 1  
prevalence.month1 <- 0.16
prevalence.month3 <- 0.42

#***********************************************************************************************************#
# Helper Functions
#***********************************************************************************************************#

# Analyze.ROC takes in a glm object and the outcome, and outputs AUC and optimal probability cutoff
# Outcome = c("m1_binaryoutcome","m3cum_binaryoutcome")
Analyze.ROC <- function(Variable.fit, Outcome){
  
  if(Outcome=="m1_binaryoutcome"){
    yhat <- as.data.frame(fitted.values(Variable.fit))
    IDs.to.keep <- m1.data %>% select(ID) %>% filter(row.names(.) %in% row.names(yhat))
    yhat.probs.ones <- data.frame(IDs.to.keep,yhat)
    colnames(yhat.probs.ones) <- c("ID","probs")
    tmp <- m1.data %>% select(ID, m1_binaryoutcome)
    data.for.roc <- left_join(yhat.probs.ones, tmp, by="ID")
    data.for.roc <- data.for.roc[complete.cases(data.for.roc),]
    
    plot.roc(data.for.roc$m1_binaryoutcome, 
             data.for.roc$probs,
             print.auc=TRUE,
             ci=TRUE,
             legacy.axes=TRUE,
             asp=NA,
             thresholds="best",
             print.thres="best",
             print.thres.best.method=optimality.criterion,
             print.thres.best.weights=c(relative.cost.assigned, prevalence.month1))
    
    
    roc.obj <- roc(data.for.roc$m1_binaryoutcome, data.for.roc$probs)
    
    
  } else if(Outcome=="m3cum_binaryoutcome"){
    yhat <- as.data.frame(fitted.values(Variable.fit))
    IDs.to.keep <- m3.data %>% select(ID) %>% filter(row.names(.) %in% row.names(yhat))
    yhat.probs.ones <- data.frame(IDs.to.keep,yhat)
    colnames(yhat.probs.ones) <- c("ID","probs")
    tmp <- m3.data %>% select(ID, m3cum_binaryoutcome)
    data.for.roc <- left_join(yhat.probs.ones, tmp, by="ID")
    data.for.roc <- data.for.roc[complete.cases(data.for.roc),]
    
    plot.roc(data.for.roc$m3cum_binaryoutcome, 
             data.for.roc$probs,
             print.auc=TRUE,
             ci=TRUE,
             legacy.axes=TRUE,
             asp=NA,
             thresholds="best",
             print.thres="best",
             print.thres.best.method=optimality.criterion,
             print.thres.best.weights=c(relative.cost.assigned, prevalence.month3))
    
    
    roc.obj <- roc(data.for.roc$m3cum_binaryoutcome, data.for.roc$probs)
    
  }
  
  return(roc.obj)
}


#***********************************************************************************************************#
# Summary statistics of cumulative week 2 features
#***********************************************************************************************************#

summary.table.cumweek2 <- m1.data %>% select(mean.Miserable.2,
                                             mean.Hopelessness.2, 
                                             mean.Burdensomeness.2, 
                                             mean.Connectedness.2,
                                             mean.Self_Efficacy.2,
                                             mean.Duration_Combined.2,
                                             var.Miserable.2, 
                                             var.Hopelessness.2, 
                                             var.Burdensomeness.2, 
                                             var.Connectedness.2,
                                             var.Self_Efficacy.2,
                                             var.Duration_Combined.2) %>%
  summarise_all(funs(mean, sd, min, max)) %>% round(digits=2) %>% t(.)

write.csv(summary.table.cumweek2, "summary.table.cumweek2.csv", row.names=TRUE)

#***********************************************************************************************************#
# Fit Logistic Regression Classifiers using mean and variance by cumulative week 2
#***********************************************************************************************************#

# Fit logistic regression model
Miserable.fit <- glm(m1_binaryoutcome ~ 
                       mean.Miserable.2 + var.Miserable.2 , 
                     data = m1.data,
                     family = "binomial"
                     )

summary(Miserable.fit)

# Compute odds ratio
coef.Miserable <- round(coef(Miserable.fit), digits=2)
se.Miserable <- round(summary(Miserable.fit)$coefficients[,2], digits=2)
odds.ratio.Miserable <- round(exp(coef(Miserable.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Miserable <- round(exp(summary(Miserable.fit)$coefficients[,1]-1.96*summary(Miserable.fit)$coefficients[,2]), digits=2)
UB.Miserable <- round(exp(summary(Miserable.fit)$coefficients[,1]+1.96*summary(Miserable.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Miserable, se.Miserable, odds.ratio.Miserable, LB.Miserable, UB.Miserable))

# Compute AUC
Analyze.ROC(Miserable.fit, "m1_binaryoutcome")

# Fit logistic regression model
Hopelessness.fit <- glm(m1_binaryoutcome ~ 
                       mean.Hopelessness.2 + var.Hopelessness.2 , 
                     data = m1.data,
                     family = "binomial"
)

summary(Hopelessness.fit)

# Compute odds ratio
coef.Hopelessness <- round(coef(Hopelessness.fit), digits=2)
se.Hopelessness <- round(summary(Hopelessness.fit)$coefficients[,2], digits=2)
odds.ratio.Hopelessness <- round(exp(coef(Hopelessness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Hopelessness <- round(exp(summary(Hopelessness.fit)$coefficients[,1]-1.96*summary(Hopelessness.fit)$coefficients[,2]), digits=2)
UB.Hopelessness <- round(exp(summary(Hopelessness.fit)$coefficients[,1]+1.96*summary(Hopelessness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Hopelessness, se.Hopelessness, odds.ratio.Hopelessness, LB.Hopelessness, UB.Hopelessness))

# Compute AUC
Analyze.ROC(Hopelessness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Burdensomeness.fit <- glm(m1_binaryoutcome ~ 
                       mean.Burdensomeness.2 + var.Burdensomeness.2 , 
                     data = m1.data,
                     family = "binomial"
)

summary(Burdensomeness.fit)

# Compute odds ratio
coef.Burdensomeness <- round(coef(Burdensomeness.fit), digits=2)
se.Burdensomeness <- round(summary(Burdensomeness.fit)$coefficients[,2], digits=2)
odds.ratio.Burdensomeness <- round(exp(coef(Burdensomeness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Burdensomeness <- round(exp(summary(Burdensomeness.fit)$coefficients[,1]-1.96*summary(Burdensomeness.fit)$coefficients[,2]), digits=2)
UB.Burdensomeness <- round(exp(summary(Burdensomeness.fit)$coefficients[,1]+1.96*summary(Burdensomeness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Burdensomeness, se.Burdensomeness, odds.ratio.Burdensomeness, LB.Burdensomeness, UB.Burdensomeness))

# Compute AUC
Analyze.ROC(Burdensomeness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Connectedness.fit <- glm(m1_binaryoutcome ~ 
                       mean.Connectedness.2 + var.Connectedness.2 , 
                     data = m1.data,
                     family = "binomial"
)

summary(Connectedness.fit)

# Compute odds ratio
coef.Connectedness <- round(coef(Connectedness.fit), digits=2)
se.Connectedness <- round(summary(Connectedness.fit)$coefficients[,2], digits=2)
odds.ratio.Connectedness <- round(exp(coef(Connectedness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Connectedness <- round(exp(summary(Connectedness.fit)$coefficients[,1]-1.96*summary(Connectedness.fit)$coefficients[,2]), digits=2)
UB.Connectedness <- round(exp(summary(Connectedness.fit)$coefficients[,1]+1.96*summary(Connectedness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Connectedness, se.Connectedness, odds.ratio.Connectedness, LB.Connectedness, UB.Connectedness))

# Compute AUC
Analyze.ROC(Connectedness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Self_Efficacy.fit <- glm(m1_binaryoutcome ~ 
                           mean.Self_Efficacy.2 + var.Self_Efficacy.2 , 
                         data = m1.data,
                         family = "binomial"
)

summary(Self_Efficacy.fit)

# Compute odds ratio
coef.Self_Efficacy <- round(coef(Self_Efficacy.fit), digits=2)
se.Self_Efficacy <- round(summary(Self_Efficacy.fit)$coefficients[,2], digits=2)
odds.ratio.Self_Efficacy <- round(exp(coef(Self_Efficacy.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Self_Efficacy <- round(exp(summary(Self_Efficacy.fit)$coefficients[,1]-1.96*summary(Self_Efficacy.fit)$coefficients[,2]), digits=2)
UB.Self_Efficacy <- round(exp(summary(Self_Efficacy.fit)$coefficients[,1]+1.96*summary(Self_Efficacy.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Self_Efficacy, se.Self_Efficacy, odds.ratio.Self_Efficacy, LB.Self_Efficacy, UB.Self_Efficacy))

# Compute AUC
Analyze.ROC(Self_Efficacy.fit, "m1_binaryoutcome")

# Fit logistic regression model
Duration_Combined.fit <- glm(m1_binaryoutcome ~ 
                           mean.Duration_Combined.2 + var.Duration_Combined.2 , 
                         data = m1.data,
                         family = "binomial"
)

summary(Duration_Combined.fit)

# Compute odds ratio
coef.Duration_Combined <- round(coef(Duration_Combined.fit), digits=2)
se.Duration_Combined <- round(summary(Duration_Combined.fit)$coefficients[,2], digits=2)
odds.ratio.Duration_Combined <- round(exp(coef(Duration_Combined.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Duration_Combined <- round(exp(summary(Duration_Combined.fit)$coefficients[,1]-1.96*summary(Duration_Combined.fit)$coefficients[,2]), digits=2)
UB.Duration_Combined <- round(exp(summary(Duration_Combined.fit)$coefficients[,1]+1.96*summary(Duration_Combined.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Duration_Combined, se.Duration_Combined, odds.ratio.Duration_Combined, LB.Duration_Combined, UB.Duration_Combined))

# Compute AUC
Analyze.ROC(Duration_Combined.fit, "m1_binaryoutcome")

# Combine estimated regression coefficients into one table
coef.table <- as.data.frame(rbind(coef.Miserable, coef.Hopelessness, 
                                  coef.Burdensomeness, coef.Connectedness, 
                                  coef.Self_Efficacy, coef.Duration_Combined))

colnames(coef.table) <- c("Intercept", "Mean", "Variance")
row.names(coef.table) <- c("Miserable","Hopelessness","Burdensomeness","Connectedness",
                           "Self-Efficacy","Duration of Suicide Ideation")
write.csv(coef.table, "coef.table.csv")

odds.ratio.table <- as.data.frame(rbind(odds.ratio.Miserable, odds.ratio.Hopelessness, 
                                        odds.ratio.Burdensomeness, odds.ratio.Connectedness, 
                                        odds.ratio.Self_Efficacy, odds.ratio.Duration_Combined))

colnames(odds.ratio.table) <- c("Intercept", "Mean", "Variance")
row.names(odds.ratio.table) <- c("Miserable","Hopelessness","Burdensomeness",
                                 "Connectedness","Self-Efficacy","Duration of Suicide Ideation")
write.csv(odds.ratio.table, "odds.ratio.table.csv")

#***********************************************************************************************************#
# Fit Logistic Regression Classifiers using mean by cumulative week 2
#***********************************************************************************************************#

# Fit logistic regression model
Miserable.fit <- glm(m1_binaryoutcome ~ 
                       mean.Miserable.2 , 
                     data = m1.data,
                     family = "binomial"
)

summary(Miserable.fit)

# Compute odds ratio
coef.Miserable <- round(coef(Miserable.fit), digits=2)
se.Miserable <- round(summary(Miserable.fit)$coefficients[,2], digits=2)
odds.ratio.Miserable <- round(exp(coef(Miserable.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Miserable <- round(exp(summary(Miserable.fit)$coefficients[,1]-1.96*summary(Miserable.fit)$coefficients[,2]), digits=2)
UB.Miserable <- round(exp(summary(Miserable.fit)$coefficients[,1]+1.96*summary(Miserable.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Miserable, se.Miserable, odds.ratio.Miserable, LB.Miserable, UB.Miserable))

# Compute AUC
Analyze.ROC(Miserable.fit, "m1_binaryoutcome")

# Fit logistic regression model
Hopelessness.fit <- glm(m1_binaryoutcome ~ 
                          mean.Hopelessness.2 , 
                        data = m1.data,
                        family = "binomial"
)

summary(Hopelessness.fit)

# Compute odds ratio
coef.Hopelessness <- round(coef(Hopelessness.fit), digits=2)
se.Hopelessness <- round(summary(Hopelessness.fit)$coefficients[,2], digits=2)
odds.ratio.Hopelessness <- round(exp(coef(Hopelessness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Hopelessness <- round(exp(summary(Hopelessness.fit)$coefficients[,1]-1.96*summary(Hopelessness.fit)$coefficients[,2]), digits=2)
UB.Hopelessness <- round(exp(summary(Hopelessness.fit)$coefficients[,1]+1.96*summary(Hopelessness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Hopelessness, se.Hopelessness, odds.ratio.Hopelessness, LB.Hopelessness, UB.Hopelessness))

# Compute AUC
Analyze.ROC(Hopelessness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Burdensomeness.fit <- glm(m1_binaryoutcome ~ 
                            mean.Burdensomeness.2 , 
                          data = m1.data,
                          family = "binomial"
)

summary(Burdensomeness.fit)

# Compute odds ratio
coef.Burdensomeness <- round(coef(Burdensomeness.fit), digits=2)
se.Burdensomeness <- round(summary(Burdensomeness.fit)$coefficients[,2], digits=2)
odds.ratio.Burdensomeness <- round(exp(coef(Burdensomeness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Burdensomeness <- round(exp(summary(Burdensomeness.fit)$coefficients[,1]-1.96*summary(Burdensomeness.fit)$coefficients[,2]), digits=2)
UB.Burdensomeness <- round(exp(summary(Burdensomeness.fit)$coefficients[,1]+1.96*summary(Burdensomeness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Burdensomeness, se.Burdensomeness, odds.ratio.Burdensomeness, LB.Burdensomeness, UB.Burdensomeness))

# Compute AUC
Analyze.ROC(Burdensomeness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Connectedness.fit <- glm(m1_binaryoutcome ~ 
                           mean.Connectedness.2 , 
                         data = m1.data,
                         family = "binomial"
)

summary(Connectedness.fit)

# Compute odds ratio
coef.Connectedness <- round(coef(Connectedness.fit), digits=2)
se.Connectedness <- round(summary(Connectedness.fit)$coefficients[,2], digits=2)
odds.ratio.Connectedness <- round(exp(coef(Connectedness.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Connectedness <- round(exp(summary(Connectedness.fit)$coefficients[,1]-1.96*summary(Connectedness.fit)$coefficients[,2]), digits=2)
UB.Connectedness <- round(exp(summary(Connectedness.fit)$coefficients[,1]+1.96*summary(Connectedness.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Connectedness, se.Connectedness, odds.ratio.Connectedness, LB.Connectedness, UB.Connectedness))

# Compute AUC
Analyze.ROC(Connectedness.fit, "m1_binaryoutcome")

# Fit logistic regression model
Self_Efficacy.fit <- glm(m1_binaryoutcome ~ 
                           mean.Self_Efficacy.2 , 
                         data = m1.data,
                         family = "binomial"
)

summary(Self_Efficacy.fit)

# Compute odds ratio
coef.Self_Efficacy <- round(coef(Self_Efficacy.fit), digits=2)
se.Self_Efficacy <- round(summary(Self_Efficacy.fit)$coefficients[,2], digits=2)
odds.ratio.Self_Efficacy <- round(exp(coef(Self_Efficacy.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Self_Efficacy <- round(exp(summary(Self_Efficacy.fit)$coefficients[,1]-1.96*summary(Self_Efficacy.fit)$coefficients[,2]), digits=2)
UB.Self_Efficacy <- round(exp(summary(Self_Efficacy.fit)$coefficients[,1]+1.96*summary(Self_Efficacy.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Self_Efficacy, se.Self_Efficacy, odds.ratio.Self_Efficacy, LB.Self_Efficacy, UB.Self_Efficacy))

# Compute AUC
Analyze.ROC(Self_Efficacy.fit, "m1_binaryoutcome")

# Fit logistic regression model
Duration_Combined.fit <- glm(m1_binaryoutcome ~ 
                               mean.Duration_Combined.2 , 
                             data = m1.data,
                             family = "binomial"
)

summary(Duration_Combined.fit)

# Compute odds ratio
coef.Duration_Combined <- round(coef(Duration_Combined.fit), digits=2)
se.Duration_Combined <- round(summary(Duration_Combined.fit)$coefficients[,2], digits=2)
odds.ratio.Duration_Combined <- round(exp(coef(Duration_Combined.fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB.Duration_Combined <- round(exp(summary(Duration_Combined.fit)$coefficients[,1]-1.96*summary(Duration_Combined.fit)$coefficients[,2]), digits=2)
UB.Duration_Combined <- round(exp(summary(Duration_Combined.fit)$coefficients[,1]+1.96*summary(Duration_Combined.fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef.Duration_Combined, se.Duration_Combined, odds.ratio.Duration_Combined, LB.Duration_Combined, UB.Duration_Combined))

# Compute AUC
Analyze.ROC(Duration_Combined.fit, "m1_binaryoutcome")

# Combine estimated regression coefficients into one table
coef.table <- as.data.frame(rbind(coef.Miserable, coef.Hopelessness, 
                                  coef.Burdensomeness, coef.Connectedness, 
                                  coef.Self_Efficacy, coef.Duration_Combined))

colnames(coef.table) <- c("Intercept", "Mean")
row.names(coef.table) <- c("Miserable","Hopelessness","Burdensomeness","Connectedness",
                           "Self-Efficacy","Duration of Suicide Ideation")
write.csv(coef.table, "coef.table.csv")

odds.ratio.table <- as.data.frame(rbind(odds.ratio.Miserable, odds.ratio.Hopelessness, 
                                        odds.ratio.Burdensomeness, odds.ratio.Connectedness, 
                                        odds.ratio.Self_Efficacy, odds.ratio.Duration_Combined))

colnames(odds.ratio.table) <- c("Intercept", "Mean")
row.names(odds.ratio.table) <- c("Miserable","Hopelessness","Burdensomeness",
                                 "Connectedness","Self-Efficacy","Duration of Suicide Ideation")
write.csv(odds.ratio.table, "odds.ratio.table.csv")


#***********************************************************************************************************#
# Fit Logistic Regression Classifiers using various cumulative week 2 variables
#***********************************************************************************************************#

# All four constructs: self-efficacy, duration of suicide ideation, hopelessness, miserable -----------------
# We do not have enough variance in the outcome for this model to work
fit <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+var.Self_Efficacy.2+
             mean.Miserable.2+var.Miserable.2+
             mean.Hopelessness.2+var.Hopelessness.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

# Three out of the four constructs: self-efficacy, duration of suicide ideation, hopelessness, miserable ----


fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Hopelessness.2+var.Hopelessness.2+
             mean.Self_Efficacy.2+var.Self_Efficacy.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Hopelessness.2+var.Hopelessness.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Self_Efficacy.2+var.Self_Efficacy.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
             mean.Self_Efficacy.2+var.Self_Efficacy.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

# Two out of the four constructs: self-efficacy, duration of suicide ideation, hopelessness, miserable ------

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Hopelessness.2+var.Hopelessness.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+var.Self_Efficacy.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+var.Self_Efficacy.2+
             mean.Miserable.2+var.Miserable.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Duration_Combined.2+var.Duration_Combined.2,
           data = m1.data,
           family = "binomial")


fit <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
             mean.Self_Efficacy.2+var.Self_Efficacy.2,
           data = m1.data,
           family = "binomial")


#***********************************************************************************************************#
# Fit Logistic Regression Classifiers using top models from previous section, but without variances
#***********************************************************************************************************#

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+
             mean.Hopelessness.2+
             mean.Self_Efficacy.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+
             mean.Self_Efficacy.2+
             mean.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+
             mean.Self_Efficacy.2+
             mean.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+
             mean.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+
             mean.Self_Efficacy.2,
           data = m1.data,
           family = "binomial")


fit <- glm(m1_binaryoutcome ~ mean.Miserable.2+mean.Hopelessness.2+
             mean.Self_Efficacy.2+mean.Duration_Combined.2,
           data = m1.data,
           family = "binomial")

# Compute odds ratio
coef <- round(coef(fit), digits=2)
se <- round(summary(fit)$coefficients[,2], digits=2)
odds.ratio <- round(exp(coef(fit)), digits=2)

# Compute 95% confidence interval for odds ratio
LB <- round(exp(summary(fit)$coefficients[,1]-1.96*summary(fit)$coefficients[,2]), digits=2)
UB <- round(exp(summary(fit)$coefficients[,1]+1.96*summary(fit)$coefficients[,2]), digits=2)

# This combines the above into a table
t(rbind(coef, se, odds.ratio, LB, UB))

# Compute AUC
Analyze.ROC(fit, "m1_binaryoutcome")

#***********************************************************************************************************#
# Statistical test to compare differences in AUC
#***********************************************************************************************************#

# Test 01 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
             mean.Hopelessness.2+var.Hopelessness.2+
             mean.Self_Efficacy.2+var.Self_Efficacy.2,
           data = m1.data,
           family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Miserable.2+
              mean.Hopelessness.2+
              mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 02 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Miserable.2+
              mean.Self_Efficacy.2+
              mean.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 03 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+
              mean.Self_Efficacy.2+
              mean.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 04 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2+
              mean.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 05 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Miserable.2+var.Miserable.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Miserable.2+mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 06 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Miserable.2+mean.Hopelessness.2+mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Miserable.2+mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 07 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Miserable.2+mean.Hopelessness.2+mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 08 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+mean.Self_Efficacy.2+mean.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong" )


# Test 09 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Self_Efficacy.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")


# Test 09 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Miserable.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 10 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

# Test 11 ---------------------------------------------------------------------------------------------------

fit1 <- glm(m1_binaryoutcome ~ mean.Hopelessness.2+var.Hopelessness.2+
              mean.Self_Efficacy.2+var.Self_Efficacy.2+
              mean.Duration_Combined.2+var.Duration_Combined.2,
            data = m1.data,
            family = "binomial")

fit2 <- glm(m1_binaryoutcome ~ mean.Burdensomeness.2,
            data = m1.data,
            family = "binomial")

roc.1 <- Analyze.ROC(fit1, "m1_binaryoutcome")

roc.2 <- Analyze.ROC(fit2, "m1_binaryoutcome")

roc.test(roc.1, roc.2, method = "delong")

#***********************************************************************************************************#
# Cut-off Region Corresponding to Threshold Determined by AUC
#***********************************************************************************************************#

p <- 0.46 # optimal cutoff


