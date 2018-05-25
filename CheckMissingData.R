# Set working directory to where raw data is saved
setwd("J:\\MI-SafeCope")

# This script lets us find out how much missing data there is and where the missing data comes from.

library(dplyr)
# The file below is created by CreateDataForAnalysis.R
dat <- read.csv("LongDataForAnalysis.csv", na.strings = c(NA))

#summary(dat)

#*********************************************************************************************************************#
# Missing Data in the Covariates                                                                                      #
#*********************************************************************************************************************#

# For each participant, count the number of days with missing data in the covariates.
# Either a participant has a response to all EMA quastions or no response to all EMA questions
# This line of code shows that half of participants have 6 days of missing data
dat %>% select(ID, Happy) %>% group_by(ID) %>%
  summarise(NumberParticipantDaysMissing = sum(is.na(Happy))) %>% 
  select(NumberParticipantDaysMissing) %>% as.matrix(.) %>% quantile(., type = 3)

# This computes the quantiles of the number of days a participant did not answer EMA's
tmp <- dat %>% select(ID, Happy) %>% group_by(ID) %>%
  summarise(NumberParticipantDaysMissing = sum(is.na(Happy))) %>% 
  select(NumberParticipantDaysMissing) %>% as.matrix(.) 

boxplot(tmp, horizontal = TRUE)
title(main = "Quantiles of No. of Days EMA's Not Answered")

# Question: On the average, how much missing data does a participant have per week?
# Do participants tend to have more missing data on some week than others?
tmp <- dat %>% select(ID, Week, Happy) %>% group_by(ID, Week) %>% 
  summarise(NumberParticipantDaysMissing = sum(isna(Happy))) %>% as.data.frame(.)

# Let's obtain the quantiles of number of days missing per participant by week
# These lines of code show that there is more missing data in the last two weeks than in the first two weeks
wk1 <- tmp %>% filter(Week == 1) %>% select(NumberParticipantDaysMissing) %>% as.matrix(.) %>% quantile(., type = 3)
wk2 <- tmp %>% filter(Week == 2) %>% select(NumberParticipantDaysMissing) %>% as.matrix(.) %>% quantile(., type = 3)
wk3 <- tmp %>% filter(Week == 3) %>% select(NumberParticipantDaysMissing) %>% as.matrix(.) %>% quantile(., type = 3)
wk4 <- tmp %>% filter(Week == 4) %>% select(NumberParticipantDaysMissing) %>% as.matrix(.) %>% quantile(., type = 3)

q1pltdat <- cbind(Week=c(1,2,3,4), Q = c(wk1[2], wk2[2], wk3[2], wk4[2]))
q2pltdat <- cbind(Week=c(1,2,3,4), Q = c(wk1[3], wk2[3], wk3[3], wk4[3]))
q3pltdat <- cbind(Week=c(1,2,3,4), Q = c(wk1[4], wk2[4], wk3[4], wk4[4]))

plot(q1pltdat, type = "o", ylim = c(0,7), xaxt = "n", ylab = "Number of Days Missing per Participant", lty = 2,
     main = "Quantiles of Number of Missing Days across Participants per Week")
axis(1, at=c(1,2,3,4))
lines(q2pltdat, type = "o", lty = 1)
lines(q3pltdat, type = "o", lty = 3)
legend(x=1,y=7, c("25th-Percentile","50th-Percentile","75th-Percentile"), lty = c(2,1,3))

# For each of the 28 days, how many participant observations do we have?
tmppltdat <- dat %>% select(Day, Happy) %>% group_by(Day) %>% 
  summarise(NumberOfParticipantObservationsAvailable = sum(is.na(Happy))) %>% as.matrix(.)


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

abline(a=12,b=0, lty=2)
abline(a=16,b=0, lty=3)
abline(a=21,b=0, lty=4)
legend(x=0,y=35, c("25th-Percentile","50th-Percentile","75th-Percentile"), lty = c(2,3,4))

#*********************************************************************************************************************#
# Missing Data in the Outcomes                                                                                        #
#*********************************************************************************************************************#

dat %>% select(ID, fm1_suicidalbeh, m1_rehospitalization, m1_returnEDvisit) %>% unique(.) %>% summary(.)
dat %>% select(ID, outcome_behavior, outcome_rehospitalization, outcome_returnEDvisit, outcome_attempt) %>% unique(.) %>% summary(.)

#*********************************************************************************************************************#
# Missing Data in the Covariates: Daily Scale, Check if Possible to do RMSSD                                                                                      #
#*********************************************************************************************************************#

# Happy

tmp <- dat %>% select(ID,Day,Happy) %>% 
  reshape(.,timevar="Day", idvar="ID",sep="_",direction="wide") %>% select(-ID)
colnames(tmp) <- c(1:28)
image(t(tmp)[,nrow(tmp):1], axes=FALSE, xlab = "Day", ylab = "One Row per Participant", col=heat.colors(6))
axis(1, at = seq(0,1,7/28), labels = c(1,7,14,21,28))
title(main="Happy (1 = Very Slightly/Not at All, 5 = Extremely)")

image(as.matrix(c(NA,1,2,3,4,5)), axes=FALSE, col=heat.colors(6))
axis(1, at = c(0,0.2,0.4,0.6,0.8,1), labels = c("NA",1,2,3,4,5))
box()

# Miserable

tmp <- dat %>% select(ID,Day,Miserable) %>% 
  reshape(.,timevar="Day", idvar="ID",sep="_",direction="wide") %>% select(-ID)
colnames(tmp) <- c(1:28)
image(t(tmp)[,nrow(tmp):1], axes=FALSE, xlab = "Day", ylab = "One Row per Participant", col=heat.colors(6))
axis(1, at = seq(0,1,7/28), labels = c(1,7,14,21,28))
title(main="Miserable (1 = Very Slightly/Not at All, 5 = Extremely)")

image(as.matrix(c(NA,1,2,3,4,5)), axes=FALSE, col=heat.colors(6))
axis(1, at = c(0,0.2,0.4,0.6,0.8,1), labels = c("NA",1,2,3,4,5))
box()

# Duration Combined

tmp <- dat %>% select(ID,Day,Duration_Combined) %>% 
  reshape(.,timevar="Day", idvar="ID",sep="_",direction="wide") %>% select(-ID)
colnames(tmp) <- c(1:28)
image(t(tmp)[,nrow(tmp):1], axes=FALSE, xlab = "Day", ylab = "One Row per Participant", col=heat.colors(7))
axis(1, at = seq(0,1,7/28), labels = c(1,7,14,21,28))
title(main="Duration of Urge in Past 24 Hours (0 = None, 1 = A Few Seconds or Minutes, 5 = More than 8 hours/continuous)")

image(as.matrix(c(NA,0,1,2,3,4,5)), axes=FALSE, col=heat.colors(7))
axis(1, at = c(0,0.16,0.32,0.48,0.64,0.80,0.96), labels = c("NA",0,1,2,3,4,5))
box()

# Frequency Combined
tmp <- dat %>% select(ID,Day,Frequency_Combined) %>% 
  reshape(.,timevar="Day", idvar="ID",sep="_",direction="wide") %>% select(-ID)

colnames(tmp) <- c(1:28)
image(t(tmp)[,nrow(tmp):1], axes=FALSE, xlab = "Day", ylab = "One Row per Participant", col=heat.colors(6))
axis(1, at = seq(0,1,7/28), labels = c(1,7,14,21,28))
title(main="Frequency of Urge in Past 24 Hours (0 = None, 1 = Only One Time, 4 = All the Time)")

image(as.matrix(c(NA,0,1,2,3,4)), axes=FALSE, col=heat.colors(6))
axis(1, at = c(0,0.2,0.4,0.6,0.8,1), labels = c("NA",0,1,2,3,4))
box()
