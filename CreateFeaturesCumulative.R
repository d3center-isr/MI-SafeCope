# Set working directory to where raw data is saved
setwd("J:\\MI-SafeCope")

library(dplyr)
library(psych)
source("C:\\Users\\jamieyap\\GitHub\\MI-SafeCope\\mssd.R")

# Read in cleaned dataset. This dataset is in long format. 
# We will need to transform this into wide format
# before we create features to be used in our classification model.
dat.long <- read.csv("LongDataForAnalysis.csv", header=TRUE, na.strings = c("NA"))

ToSumRows <- function(OneRow){
  if(sum(is.na(OneRow)) == length(OneRow)){
    total <- NA
  }else{
    total <- sum(OneRow,na.rm=TRUE)
  }
  return(total)
}

response.df <- dat.long %>% select(ID, Female, Intervention,
                                   B_multipleattempt, B_attemptyesno,
                                   fm1_suicidalbeh, m1_rehospitalization,
                                   m1_returnEDvisit, m1_binaryoutcome,
                                   outcome_returnEDvisit, outcome_attempt,
                                   outcome_behavior,outcome_rehospitalization,
                                   m3cum_binaryoutcome) %>% unique(.)

# Create the variable CumWeek1, CumWeek2, CumWeek3, CumWeek4

dat.long <- dat.long %>% mutate(CumWeek1 = if_else(Week==1,1,0)) %>%
  mutate(CumWeek2 = if_else((Week==1)|(Week==2),1,0)) %>%
  mutate(CumWeek3 = if_else((Week==1)|(Week==2)|(Week==3),1,0)) %>%
  mutate(CumWeek4 = if_else((Week==1)|(Week==2)|(Week==3)|(Week==4),1,0))

dat.long.CumWeek1 <- dat.long %>% filter(CumWeek1==1) %>% select(-CumWeek1)
dat.long.CumWeek2 <- dat.long %>% filter(CumWeek2==1) %>% select(-CumWeek2)
dat.long.CumWeek3 <- dat.long %>% filter(CumWeek3==1) %>% select(-CumWeek3)
dat.long.CumWeek4 <- dat.long %>% filter(CumWeek4==1) %>% select(-CumWeek4)

#**************************************************************************************************#
# Construct variables at CumWeek1
#**************************************************************************************************#

variables.CumWeek1 <- dat.long.CumWeek1 %>%  
  select(ID, Happy, Miserable, Angry, Hopelessness, 
         Burdensomeness, Connectedness, Self_Efficacy, SI, 
         NSSI, Urge_Combined, Frequency_Combined, Duration_Combined,
         ConstructiveCoping, DestructiveCoping, AnyCoping, AnyCopingAndHelpful) %>%
  group_by(ID) %>%
  summarise(mean.Happy = mean(Happy, na.rm=TRUE), 
            mean.Miserable = mean(Miserable, na.rm=TRUE),
            mean.Angry = mean(Angry, na.rm=TRUE),
            mean.Hopelessness = mean(Hopelessness, na.rm=TRUE),
            mean.Burdensomeness = mean(Burdensomeness, na.rm=TRUE),
            mean.Connectedness = mean(Connectedness, na.rm=TRUE),
            mean.Self_Efficacy = mean(Self_Efficacy, na.rm=TRUE),
            mean.SI = mean(SI, na.rm=TRUE),
            mean.NSSI = mean(NSSI, na.rm=TRUE),
            mean.Urge_Combined = mean(Urge_Combined, na.rm=TRUE),
            mean.Frequency_Combined = mean(Frequency_Combined, na.rm=TRUE),
            mean.Duration_Combined = mean(Duration_Combined, na.rm=TRUE),
            #mean.ConstructiveCoping = mean(ConstructiveCoping, na.rm=TRUE),
            #mean.DestructiveCoping = mean(DestructiveCoping, na.rm=TRUE),
            #mean.AnyCoping = mean(AnyCoping, na.rm=TRUE),
            #mean.AnyCopingAndHelpful = mean(AnyCopingAndHelpful, na.rm=TRUE),
            var.Happy = var(Happy, na.rm=TRUE),
            var.Miserable = var(Miserable, na.rm=TRUE),
            var.Angry = var(Angry, na.rm=TRUE),
            var.Hopelessness = var(Hopelessness, na.rm=TRUE),
            var.Burdensomeness = var(Burdensomeness, na.rm=TRUE),
            var.Connectedness = var(Connectedness, na.rm=TRUE),
            var.Self_Efficacy = var(Self_Efficacy, na.rm=TRUE),
            #var.SI = var(SI, na.rm=TRUE),
            #var.NSSI = var(NSSI, na.rm=TRUE),
            var.Urge_Combined = var(Urge_Combined, na.rm=TRUE),
            var.Frequency_Combined = var(Frequency_Combined, na.rm=TRUE),
            var.Duration_Combined = var(Duration_Combined, na.rm=TRUE),
            #var.ConstructiveCoping = var(ConstructiveCoping, na.rm=TRUE),
            #var.DestructiveCoping = var(DestructiveCoping, na.rm=TRUE),
            #var.AnyCoping = var(AnyCoping, na.rm=TRUE),
            #var.AnyCopingAndHelpful = var(AnyCopingAndHelpful, na.rm=TRUE),
            #miss.Happy = sum(is.na(Happy)),
            #miss.Miserable = sum(is.na(Miserable)),
            #miss.Angry = sum(is.na(Angry)),
            #miss.Hopelessness = sum(is.na(Hopelessness)),
            #miss.Burdensomeness = sum(is.na(Burdensomeness)),
            #miss.Connectedness = sum(is.na(Connectedness)),
            #miss.Self_Efficacy = sum(is.na(Self_Efficacy)),
            miss.SI = sum(is.na(SI)),
            #miss.NSSI = sum(is.na(NSSI)),
            #miss.Urge_Combined = sum(is.na(Urge_Combined)),
            #miss.Frequency_Combined = sum(is.na(Frequency_Combined)),
            #miss.Duration_Combined = sum(is.na(Duration_Combined)),
            #miss.ConstructiveCoping = sum(is.na(ConstructiveCoping)),
            #miss.DestructiveCoping = sum(is.na(DestructiveCoping)),
            #miss.AnyCoping = sum(is.na(AnyCoping)),
            #miss.AnyCopingAndHelpful = sum(is.na(AnyCopingAndHelpful)),
            max.Happy = max(Happy, na.rm=TRUE),
            max.Miserable = max(Miserable, na.rm=TRUE),
            max.Angry = max(Angry, na.rm=TRUE),
            max.Hopelessness = max(Hopelessness, na.rm=TRUE),
            max.Burdensomeness = max(Burdensomeness, na.rm=TRUE),
            max.Connectedness = max(Connectedness, na.rm=TRUE),
            max.Self_Efficacy = max(Self_Efficacy, na.rm=TRUE),
            #max.SI = max(SI, na.rm=TRUE),
            #max.NSSI = max(NSSI, na.rm=TRUE),
            max.Urge_Combined = max(Urge_Combined, na.rm=TRUE),
            max.Frequency_Combined = max(Frequency_Combined, na.rm=TRUE),
            max.Duration_Combined = max(Duration_Combined, na.rm=TRUE),
            #max.ConstructiveCoping = max(ConstructiveCoping, na.rm=TRUE),
            #max.DestructiveCoping = max(DestructiveCoping, na.rm=TRUE),
            #max.AnyCoping = max(AnyCoping, na.rm=TRUE),
            #max.AnyCopingAndHelpful = max(AnyCopingAndHelpful, na.rm=TRUE),
            min.Happy = min(Happy, na.rm=TRUE),
            min.Miserable = min(Miserable, na.rm=TRUE),
            min.Angry = min(Angry, na.rm=TRUE),
            min.Hopelessness = min(Hopelessness, na.rm=TRUE),
            min.Burdensomeness = min(Burdensomeness, na.rm=TRUE),
            min.Connectedness = min(Connectedness, na.rm=TRUE),
            min.Self_Efficacy = min(Self_Efficacy, na.rm=TRUE),
            #min.SI = min(SI, na.rm=TRUE),
            #min.NSSI = min(NSSI, na.rm=TRUE),
            min.Urge_Combined = min(Urge_Combined, na.rm=TRUE),
            min.Frequency_Combined = min(Frequency_Combined, na.rm=TRUE),
            min.Duration_Combined = min(Duration_Combined, na.rm=TRUE),
            #min.ConstructiveCoping = min(ConstructiveCoping, na.rm=TRUE),
            #min.DestructiveCoping = min(DestructiveCoping, na.rm=TRUE),
            #min.AnyCoping = min(AnyCoping, na.rm=TRUE),
            #min.AnyCopingAndHelpful = min(AnyCopingAndHelpful, na.rm=TRUE),
            #sum.SI = ToSumRows(SI),
            #sum.NSSI = ToSumRows(NSSI),
            #sum.AnyCoping = ToSumRows(AnyCoping),
            #sum.AnyCopingAndHelpful = ToSumRows(AnyCopingAndHelpful),
            rmssd.Happy = rmssd(Happy, lag=1, na.rm=TRUE),
            rmssd.Miserable = rmssd(Miserable, lag=1, na.rm=TRUE),
            rmssd.Angry = rmssd(Angry, lag=1, na.rm=TRUE),
            rmssd.Hopelessness = rmssd(Hopelessness, lag=1, na.rm=TRUE),
            rmssd.Burdensomeness = rmssd(Burdensomeness, lag=1, na.rm=TRUE),
            rmssd.Connectedness = rmssd(Connectedness, lag=1, na.rm=TRUE),
            rmssd.Self_Efficacy = rmssd(Self_Efficacy, lag=1, na.rm=TRUE),
            #rmssd.SI = rmssd(SI, lag=1, na.rm=TRUE),
            #rmssd.NSSI = rmssd(NSSI, lag=1, na.rm=TRUE),
            rmssd.Urge_Combined = rmssd(Urge_Combined, lag=1, na.rm=TRUE),
            rmssd.Frequency_Combined = rmssd(Frequency_Combined, lag=1, na.rm=TRUE),
            rmssd.Duration_Combined = rmssd(Duration_Combined, lag=1, na.rm=TRUE)
            #rmssd.ConstructiveCoping = rmssd(ConstructiveCoping, lag=1, na.rm=TRUE),
            #rmssd.DestructiveCoping = rmssd(DestructiveCoping, lag=1, na.rm=TRUE)
            #rmssd.AnyCoping = rmssd(AnyCoping, lag=1, na.rm=TRUE),
            #rmssd.AnyCopingAndHelpful = rmssd(AnyCopingAndHelpful, lag=1, na.rm=TRUE)
  ) %>% as.data.frame(.)

variables.CumWeek1 <- left_join(variables.CumWeek1, response.df, by = c("ID"))

write.csv(variables.CumWeek1, "Week1CumulativeData.csv", row.names = FALSE)

corr.CumWeek1 <- variables.CumWeek1 %>% select(-ID) %>% cor(., use = "pairwise.complete.obs")

write.csv(corr.CumWeek1,"corr.CumWeek1.csv")


#**************************************************************************************************#
# Construct variables at CumWeek2
#**************************************************************************************************#

variables.CumWeek2 <- dat.long.CumWeek2 %>%  
  select(ID, Happy, Miserable, Angry, Hopelessness, 
         Burdensomeness, Connectedness, Self_Efficacy, SI, 
         NSSI, Urge_Combined, Frequency_Combined, Duration_Combined,
         ConstructiveCoping, DestructiveCoping, AnyCoping, AnyCopingAndHelpful) %>%
  group_by(ID) %>%
  summarise(mean.Happy = mean(Happy, na.rm=TRUE), 
            mean.Miserable = mean(Miserable, na.rm=TRUE),
            mean.Angry = mean(Angry, na.rm=TRUE),
            mean.Hopelessness = mean(Hopelessness, na.rm=TRUE),
            mean.Burdensomeness = mean(Burdensomeness, na.rm=TRUE),
            mean.Connectedness = mean(Connectedness, na.rm=TRUE),
            mean.Self_Efficacy = mean(Self_Efficacy, na.rm=TRUE),
            mean.SI = mean(SI, na.rm=TRUE),
            mean.NSSI = mean(NSSI, na.rm=TRUE),
            mean.Urge_Combined = mean(Urge_Combined, na.rm=TRUE),
            mean.Frequency_Combined = mean(Frequency_Combined, na.rm=TRUE),
            mean.Duration_Combined = mean(Duration_Combined, na.rm=TRUE),
            #mean.ConstructiveCoping = mean(ConstructiveCoping, na.rm=TRUE),
            #mean.DestructiveCoping = mean(DestructiveCoping, na.rm=TRUE),
            #mean.AnyCoping = mean(AnyCoping, na.rm=TRUE),
            #mean.AnyCopingAndHelpful = mean(AnyCopingAndHelpful, na.rm=TRUE),
            var.Happy = var(Happy, na.rm=TRUE),
            var.Miserable = var(Miserable, na.rm=TRUE),
            var.Angry = var(Angry, na.rm=TRUE),
            var.Hopelessness = var(Hopelessness, na.rm=TRUE),
            var.Burdensomeness = var(Burdensomeness, na.rm=TRUE),
            var.Connectedness = var(Connectedness, na.rm=TRUE),
            var.Self_Efficacy = var(Self_Efficacy, na.rm=TRUE),
            #var.SI = var(SI, na.rm=TRUE),
            #var.NSSI = var(NSSI, na.rm=TRUE),
            var.Urge_Combined = var(Urge_Combined, na.rm=TRUE),
            var.Frequency_Combined = var(Frequency_Combined, na.rm=TRUE),
            var.Duration_Combined = var(Duration_Combined, na.rm=TRUE),
            #var.ConstructiveCoping = var(ConstructiveCoping, na.rm=TRUE),
            #var.DestructiveCoping = var(DestructiveCoping, na.rm=TRUE),
            #var.AnyCoping = var(AnyCoping, na.rm=TRUE),
            #var.AnyCopingAndHelpful = var(AnyCopingAndHelpful, na.rm=TRUE),
            #miss.Happy = sum(is.na(Happy)),
            #miss.Miserable = sum(is.na(Miserable)),
            #miss.Angry = sum(is.na(Angry)),
            #miss.Hopelessness = sum(is.na(Hopelessness)),
            #miss.Burdensomeness = sum(is.na(Burdensomeness)),
            #miss.Connectedness = sum(is.na(Connectedness)),
            #miss.Self_Efficacy = sum(is.na(Self_Efficacy)),
            miss.SI = sum(is.na(SI)),
            #miss.NSSI = sum(is.na(NSSI)),
            #miss.Urge_Combined = sum(is.na(Urge_Combined)),
            #miss.Frequency_Combined = sum(is.na(Frequency_Combined)),
            #miss.Duration_Combined = sum(is.na(Duration_Combined)),
            #miss.ConstructiveCoping = sum(is.na(ConstructiveCoping)),
            #miss.DestructiveCoping = sum(is.na(DestructiveCoping)),
            #miss.AnyCoping = sum(is.na(AnyCoping)),
            #miss.AnyCopingAndHelpful = sum(is.na(AnyCopingAndHelpful)),
            max.Happy = max(Happy, na.rm=TRUE),
            max.Miserable = max(Miserable, na.rm=TRUE),
            max.Angry = max(Angry, na.rm=TRUE),
            max.Hopelessness = max(Hopelessness, na.rm=TRUE),
            max.Burdensomeness = max(Burdensomeness, na.rm=TRUE),
            max.Connectedness = max(Connectedness, na.rm=TRUE),
            max.Self_Efficacy = max(Self_Efficacy, na.rm=TRUE),
            #max.SI = max(SI, na.rm=TRUE),
            #max.NSSI = max(NSSI, na.rm=TRUE),
            max.Urge_Combined = max(Urge_Combined, na.rm=TRUE),
            max.Frequency_Combined = max(Frequency_Combined, na.rm=TRUE),
            max.Duration_Combined = max(Duration_Combined, na.rm=TRUE),
            #max.ConstructiveCoping = max(ConstructiveCoping, na.rm=TRUE),
            #max.DestructiveCoping = max(DestructiveCoping, na.rm=TRUE),
            #max.AnyCoping = max(AnyCoping, na.rm=TRUE),
            #max.AnyCopingAndHelpful = max(AnyCopingAndHelpful, na.rm=TRUE),
            min.Happy = min(Happy, na.rm=TRUE),
            min.Miserable = min(Miserable, na.rm=TRUE),
            min.Angry = min(Angry, na.rm=TRUE),
            min.Hopelessness = min(Hopelessness, na.rm=TRUE),
            min.Burdensomeness = min(Burdensomeness, na.rm=TRUE),
            min.Connectedness = min(Connectedness, na.rm=TRUE),
            min.Self_Efficacy = min(Self_Efficacy, na.rm=TRUE),
            #min.SI = min(SI, na.rm=TRUE),
            #min.NSSI = min(NSSI, na.rm=TRUE),
            min.Urge_Combined = min(Urge_Combined, na.rm=TRUE),
            min.Frequency_Combined = min(Frequency_Combined, na.rm=TRUE),
            min.Duration_Combined = min(Duration_Combined, na.rm=TRUE),
            #min.ConstructiveCoping = min(ConstructiveCoping, na.rm=TRUE),
            #min.DestructiveCoping = min(DestructiveCoping, na.rm=TRUE),
            #min.AnyCoping = min(AnyCoping, na.rm=TRUE),
            #min.AnyCopingAndHelpful = min(AnyCopingAndHelpful, na.rm=TRUE),
            #sum.SI = ToSumRows(SI),
            #sum.NSSI = ToSumRows(NSSI),
            #sum.AnyCoping = ToSumRows(AnyCoping),
            #sum.AnyCopingAndHelpful = ToSumRows(AnyCopingAndHelpful),
            rmssd.Happy = rmssd(Happy, lag=1, na.rm=TRUE),
            rmssd.Miserable = rmssd(Miserable, lag=1, na.rm=TRUE),
            rmssd.Angry = rmssd(Angry, lag=1, na.rm=TRUE),
            rmssd.Hopelessness = rmssd(Hopelessness, lag=1, na.rm=TRUE),
            rmssd.Burdensomeness = rmssd(Burdensomeness, lag=1, na.rm=TRUE),
            rmssd.Connectedness = rmssd(Connectedness, lag=1, na.rm=TRUE),
            rmssd.Self_Efficacy = rmssd(Self_Efficacy, lag=1, na.rm=TRUE),
            #rmssd.SI = rmssd(SI, lag=1, na.rm=TRUE),
            #rmssd.NSSI = rmssd(NSSI, lag=1, na.rm=TRUE),
            rmssd.Urge_Combined = rmssd(Urge_Combined, lag=1, na.rm=TRUE),
            rmssd.Frequency_Combined = rmssd(Frequency_Combined, lag=1, na.rm=TRUE),
            rmssd.Duration_Combined = rmssd(Duration_Combined, lag=1, na.rm=TRUE)
            #rmssd.ConstructiveCoping = rmssd(ConstructiveCoping, lag=1, na.rm=TRUE),
            #rmssd.DestructiveCoping = rmssd(DestructiveCoping, lag=1, na.rm=TRUE)
            #rmssd.AnyCoping = rmssd(AnyCoping, lag=1, na.rm=TRUE),
            #rmssd.AnyCopingAndHelpful = rmssd(AnyCopingAndHelpful, lag=1, na.rm=TRUE)
  ) %>% as.data.frame(.)

variables.CumWeek2 <- left_join(variables.CumWeek2, response.df, by = c("ID"))

write.csv(variables.CumWeek2, "Week2CumulativeData.csv", row.names = FALSE)

corr.CumWeek2 <- variables.CumWeek2 %>% select(-ID) %>% cor(., use = "pairwise.complete.obs")

write.csv(corr.CumWeek2,"corr.CumWeek2.csv")

#**************************************************************************************************#
# Construct variables at CumWeek3
#**************************************************************************************************#


variables.CumWeek3 <- dat.long.CumWeek3 %>%  
  select(ID, Happy, Miserable, Angry, Hopelessness, 
         Burdensomeness, Connectedness, Self_Efficacy, SI, 
         NSSI, Urge_Combined, Frequency_Combined, Duration_Combined,
         ConstructiveCoping, DestructiveCoping, AnyCoping, AnyCopingAndHelpful) %>%
  group_by(ID) %>%
  summarise(mean.Happy = mean(Happy, na.rm=TRUE), 
            mean.Miserable = mean(Miserable, na.rm=TRUE),
            mean.Angry = mean(Angry, na.rm=TRUE),
            mean.Hopelessness = mean(Hopelessness, na.rm=TRUE),
            mean.Burdensomeness = mean(Burdensomeness, na.rm=TRUE),
            mean.Connectedness = mean(Connectedness, na.rm=TRUE),
            mean.Self_Efficacy = mean(Self_Efficacy, na.rm=TRUE),
            mean.SI = mean(SI, na.rm=TRUE),
            mean.NSSI = mean(NSSI, na.rm=TRUE),
            mean.Urge_Combined = mean(Urge_Combined, na.rm=TRUE),
            mean.Frequency_Combined = mean(Frequency_Combined, na.rm=TRUE),
            mean.Duration_Combined = mean(Duration_Combined, na.rm=TRUE),
            #mean.ConstructiveCoping = mean(ConstructiveCoping, na.rm=TRUE),
            #mean.DestructiveCoping = mean(DestructiveCoping, na.rm=TRUE),
            #mean.AnyCoping = mean(AnyCoping, na.rm=TRUE),
            #mean.AnyCopingAndHelpful = mean(AnyCopingAndHelpful, na.rm=TRUE),
            var.Happy = var(Happy, na.rm=TRUE),
            var.Miserable = var(Miserable, na.rm=TRUE),
            var.Angry = var(Angry, na.rm=TRUE),
            var.Hopelessness = var(Hopelessness, na.rm=TRUE),
            var.Burdensomeness = var(Burdensomeness, na.rm=TRUE),
            var.Connectedness = var(Connectedness, na.rm=TRUE),
            var.Self_Efficacy = var(Self_Efficacy, na.rm=TRUE),
            #var.SI = var(SI, na.rm=TRUE),
            #var.NSSI = var(NSSI, na.rm=TRUE),
            var.Urge_Combined = var(Urge_Combined, na.rm=TRUE),
            var.Frequency_Combined = var(Frequency_Combined, na.rm=TRUE),
            var.Duration_Combined = var(Duration_Combined, na.rm=TRUE),
            #var.ConstructiveCoping = var(ConstructiveCoping, na.rm=TRUE),
            #var.DestructiveCoping = var(DestructiveCoping, na.rm=TRUE),
            #var.AnyCoping = var(AnyCoping, na.rm=TRUE),
            #var.AnyCopingAndHelpful = var(AnyCopingAndHelpful, na.rm=TRUE),
            #miss.Happy = sum(is.na(Happy)),
            #miss.Miserable = sum(is.na(Miserable)),
            #miss.Angry = sum(is.na(Angry)),
            #miss.Hopelessness = sum(is.na(Hopelessness)),
            #miss.Burdensomeness = sum(is.na(Burdensomeness)),
            #miss.Connectedness = sum(is.na(Connectedness)),
            #miss.Self_Efficacy = sum(is.na(Self_Efficacy)),
            miss.SI = sum(is.na(SI)),
            #miss.NSSI = sum(is.na(NSSI)),
            #miss.Urge_Combined = sum(is.na(Urge_Combined)),
            #miss.Frequency_Combined = sum(is.na(Frequency_Combined)),
            #miss.Duration_Combined = sum(is.na(Duration_Combined)),
            #miss.ConstructiveCoping = sum(is.na(ConstructiveCoping)),
            #miss.DestructiveCoping = sum(is.na(DestructiveCoping)),
            #miss.AnyCoping = sum(is.na(AnyCoping)),
            #miss.AnyCopingAndHelpful = sum(is.na(AnyCopingAndHelpful)),
            max.Happy = max(Happy, na.rm=TRUE),
            max.Miserable = max(Miserable, na.rm=TRUE),
            max.Angry = max(Angry, na.rm=TRUE),
            max.Hopelessness = max(Hopelessness, na.rm=TRUE),
            max.Burdensomeness = max(Burdensomeness, na.rm=TRUE),
            max.Connectedness = max(Connectedness, na.rm=TRUE),
            max.Self_Efficacy = max(Self_Efficacy, na.rm=TRUE),
            #max.SI = max(SI, na.rm=TRUE),
            #max.NSSI = max(NSSI, na.rm=TRUE),
            max.Urge_Combined = max(Urge_Combined, na.rm=TRUE),
            max.Frequency_Combined = max(Frequency_Combined, na.rm=TRUE),
            max.Duration_Combined = max(Duration_Combined, na.rm=TRUE),
            #max.ConstructiveCoping = max(ConstructiveCoping, na.rm=TRUE),
            #max.DestructiveCoping = max(DestructiveCoping, na.rm=TRUE),
            #max.AnyCoping = max(AnyCoping, na.rm=TRUE),
            #max.AnyCopingAndHelpful = max(AnyCopingAndHelpful, na.rm=TRUE),
            min.Happy = min(Happy, na.rm=TRUE),
            min.Miserable = min(Miserable, na.rm=TRUE),
            min.Angry = min(Angry, na.rm=TRUE),
            min.Hopelessness = min(Hopelessness, na.rm=TRUE),
            min.Burdensomeness = min(Burdensomeness, na.rm=TRUE),
            min.Connectedness = min(Connectedness, na.rm=TRUE),
            min.Self_Efficacy = min(Self_Efficacy, na.rm=TRUE),
            #min.SI = min(SI, na.rm=TRUE),
            #min.NSSI = min(NSSI, na.rm=TRUE),
            min.Urge_Combined = min(Urge_Combined, na.rm=TRUE),
            min.Frequency_Combined = min(Frequency_Combined, na.rm=TRUE),
            min.Duration_Combined = min(Duration_Combined, na.rm=TRUE),
            #min.ConstructiveCoping = min(ConstructiveCoping, na.rm=TRUE),
            #min.DestructiveCoping = min(DestructiveCoping, na.rm=TRUE),
            #min.AnyCoping = min(AnyCoping, na.rm=TRUE),
            #min.AnyCopingAndHelpful = min(AnyCopingAndHelpful, na.rm=TRUE),
            #sum.SI = ToSumRows(SI),
            #sum.NSSI = ToSumRows(NSSI),
            #sum.AnyCoping = ToSumRows(AnyCoping),
            #sum.AnyCopingAndHelpful = ToSumRows(AnyCopingAndHelpful),
            rmssd.Happy = rmssd(Happy, lag=1, na.rm=TRUE),
            rmssd.Miserable = rmssd(Miserable, lag=1, na.rm=TRUE),
            rmssd.Angry = rmssd(Angry, lag=1, na.rm=TRUE),
            rmssd.Hopelessness = rmssd(Hopelessness, lag=1, na.rm=TRUE),
            rmssd.Burdensomeness = rmssd(Burdensomeness, lag=1, na.rm=TRUE),
            rmssd.Connectedness = rmssd(Connectedness, lag=1, na.rm=TRUE),
            rmssd.Self_Efficacy = rmssd(Self_Efficacy, lag=1, na.rm=TRUE),
            #rmssd.SI = rmssd(SI, lag=1, na.rm=TRUE),
            #rmssd.NSSI = rmssd(NSSI, lag=1, na.rm=TRUE),
            rmssd.Urge_Combined = rmssd(Urge_Combined, lag=1, na.rm=TRUE),
            rmssd.Frequency_Combined = rmssd(Frequency_Combined, lag=1, na.rm=TRUE),
            rmssd.Duration_Combined = rmssd(Duration_Combined, lag=1, na.rm=TRUE)
            #rmssd.ConstructiveCoping = rmssd(ConstructiveCoping, lag=1, na.rm=TRUE),
            #rmssd.DestructiveCoping = rmssd(DestructiveCoping, lag=1, na.rm=TRUE)
            #rmssd.AnyCoping = rmssd(AnyCoping, lag=1, na.rm=TRUE),
            #rmssd.AnyCopingAndHelpful = rmssd(AnyCopingAndHelpful, lag=1, na.rm=TRUE)
  ) %>% as.data.frame(.)

variables.CumWeek3 <- left_join(variables.CumWeek3, response.df, by = c("ID"))

write.csv(variables.CumWeek3, "Week3CumulativeData.csv", row.names = FALSE)

corr.CumWeek3 <- variables.CumWeek3 %>% select(-ID) %>% cor(., use = "pairwise.complete.obs")

write.csv(corr.CumWeek3,"corr.CumWeek3.csv")


#**************************************************************************************************#
# Construct variables at CumWeek4
#**************************************************************************************************#

variables.CumWeek4 <- dat.long.CumWeek4 %>%  
  select(ID, Happy, Miserable, Angry, Hopelessness, 
         Burdensomeness, Connectedness, Self_Efficacy, SI, 
         NSSI, Urge_Combined, Frequency_Combined, Duration_Combined,
         ConstructiveCoping, DestructiveCoping, AnyCoping, AnyCopingAndHelpful) %>%
  group_by(ID) %>%
  summarise(mean.Happy = mean(Happy, na.rm=TRUE), 
            mean.Miserable = mean(Miserable, na.rm=TRUE),
            mean.Angry = mean(Angry, na.rm=TRUE),
            mean.Hopelessness = mean(Hopelessness, na.rm=TRUE),
            mean.Burdensomeness = mean(Burdensomeness, na.rm=TRUE),
            mean.Connectedness = mean(Connectedness, na.rm=TRUE),
            mean.Self_Efficacy = mean(Self_Efficacy, na.rm=TRUE),
            mean.SI = mean(SI, na.rm=TRUE),
            mean.NSSI = mean(NSSI, na.rm=TRUE),
            mean.Urge_Combined = mean(Urge_Combined, na.rm=TRUE),
            mean.Frequency_Combined = mean(Frequency_Combined, na.rm=TRUE),
            mean.Duration_Combined = mean(Duration_Combined, na.rm=TRUE),
            #mean.ConstructiveCoping = mean(ConstructiveCoping, na.rm=TRUE),
            #mean.DestructiveCoping = mean(DestructiveCoping, na.rm=TRUE),
            #mean.AnyCoping = mean(AnyCoping, na.rm=TRUE),
            #mean.AnyCopingAndHelpful = mean(AnyCopingAndHelpful, na.rm=TRUE),
            var.Happy = var(Happy, na.rm=TRUE),
            var.Miserable = var(Miserable, na.rm=TRUE),
            var.Angry = var(Angry, na.rm=TRUE),
            var.Hopelessness = var(Hopelessness, na.rm=TRUE),
            var.Burdensomeness = var(Burdensomeness, na.rm=TRUE),
            var.Connectedness = var(Connectedness, na.rm=TRUE),
            var.Self_Efficacy = var(Self_Efficacy, na.rm=TRUE),
            #var.SI = var(SI, na.rm=TRUE),
            #var.NSSI = var(NSSI, na.rm=TRUE),
            var.Urge_Combined = var(Urge_Combined, na.rm=TRUE),
            var.Frequency_Combined = var(Frequency_Combined, na.rm=TRUE),
            var.Duration_Combined = var(Duration_Combined, na.rm=TRUE),
            #var.ConstructiveCoping = var(ConstructiveCoping, na.rm=TRUE),
            #var.DestructiveCoping = var(DestructiveCoping, na.rm=TRUE),
            #var.AnyCoping = var(AnyCoping, na.rm=TRUE),
            #var.AnyCopingAndHelpful = var(AnyCopingAndHelpful, na.rm=TRUE),
            #miss.Happy = sum(is.na(Happy)),
            #miss.Miserable = sum(is.na(Miserable)),
            #miss.Angry = sum(is.na(Angry)),
            #miss.Hopelessness = sum(is.na(Hopelessness)),
            #miss.Burdensomeness = sum(is.na(Burdensomeness)),
            #miss.Connectedness = sum(is.na(Connectedness)),
            #miss.Self_Efficacy = sum(is.na(Self_Efficacy)),
            miss.SI = sum(is.na(SI)),
            #miss.NSSI = sum(is.na(NSSI)),
            #miss.Urge_Combined = sum(is.na(Urge_Combined)),
            #miss.Frequency_Combined = sum(is.na(Frequency_Combined)),
            #miss.Duration_Combined = sum(is.na(Duration_Combined)),
            #miss.ConstructiveCoping = sum(is.na(ConstructiveCoping)),
            #miss.DestructiveCoping = sum(is.na(DestructiveCoping)),
            #miss.AnyCoping = sum(is.na(AnyCoping)),
            #miss.AnyCopingAndHelpful = sum(is.na(AnyCopingAndHelpful)),
            max.Happy = max(Happy, na.rm=TRUE),
            max.Miserable = max(Miserable, na.rm=TRUE),
            max.Angry = max(Angry, na.rm=TRUE),
            max.Hopelessness = max(Hopelessness, na.rm=TRUE),
            max.Burdensomeness = max(Burdensomeness, na.rm=TRUE),
            max.Connectedness = max(Connectedness, na.rm=TRUE),
            max.Self_Efficacy = max(Self_Efficacy, na.rm=TRUE),
            #max.SI = max(SI, na.rm=TRUE),
            #max.NSSI = max(NSSI, na.rm=TRUE),
            max.Urge_Combined = max(Urge_Combined, na.rm=TRUE),
            max.Frequency_Combined = max(Frequency_Combined, na.rm=TRUE),
            max.Duration_Combined = max(Duration_Combined, na.rm=TRUE),
            #max.ConstructiveCoping = max(ConstructiveCoping, na.rm=TRUE),
            #max.DestructiveCoping = max(DestructiveCoping, na.rm=TRUE),
            #max.AnyCoping = max(AnyCoping, na.rm=TRUE),
            #max.AnyCopingAndHelpful = max(AnyCopingAndHelpful, na.rm=TRUE),
            min.Happy = min(Happy, na.rm=TRUE),
            min.Miserable = min(Miserable, na.rm=TRUE),
            min.Angry = min(Angry, na.rm=TRUE),
            min.Hopelessness = min(Hopelessness, na.rm=TRUE),
            min.Burdensomeness = min(Burdensomeness, na.rm=TRUE),
            min.Connectedness = min(Connectedness, na.rm=TRUE),
            min.Self_Efficacy = min(Self_Efficacy, na.rm=TRUE),
            #min.SI = min(SI, na.rm=TRUE),
            #min.NSSI = min(NSSI, na.rm=TRUE),
            min.Urge_Combined = min(Urge_Combined, na.rm=TRUE),
            min.Frequency_Combined = min(Frequency_Combined, na.rm=TRUE),
            min.Duration_Combined = min(Duration_Combined, na.rm=TRUE),
            #min.ConstructiveCoping = min(ConstructiveCoping, na.rm=TRUE),
            #min.DestructiveCoping = min(DestructiveCoping, na.rm=TRUE),
            #min.AnyCoping = min(AnyCoping, na.rm=TRUE),
            #min.AnyCopingAndHelpful = min(AnyCopingAndHelpful, na.rm=TRUE),
            #sum.SI = ToSumRows(SI),
            #sum.NSSI = ToSumRows(NSSI),
            #sum.AnyCoping = ToSumRows(AnyCoping),
            #sum.AnyCopingAndHelpful = ToSumRows(AnyCopingAndHelpful),
            rmssd.Happy = rmssd(Happy, lag=1, na.rm=TRUE),
            rmssd.Miserable = rmssd(Miserable, lag=1, na.rm=TRUE),
            rmssd.Angry = rmssd(Angry, lag=1, na.rm=TRUE),
            rmssd.Hopelessness = rmssd(Hopelessness, lag=1, na.rm=TRUE),
            rmssd.Burdensomeness = rmssd(Burdensomeness, lag=1, na.rm=TRUE),
            rmssd.Connectedness = rmssd(Connectedness, lag=1, na.rm=TRUE),
            rmssd.Self_Efficacy = rmssd(Self_Efficacy, lag=1, na.rm=TRUE),
            #rmssd.SI = rmssd(SI, lag=1, na.rm=TRUE),
            #rmssd.NSSI = rmssd(NSSI, lag=1, na.rm=TRUE),
            rmssd.Urge_Combined = rmssd(Urge_Combined, lag=1, na.rm=TRUE),
            rmssd.Frequency_Combined = rmssd(Frequency_Combined, lag=1, na.rm=TRUE),
            rmssd.Duration_Combined = rmssd(Duration_Combined, lag=1, na.rm=TRUE)
            #rmssd.ConstructiveCoping = rmssd(ConstructiveCoping, lag=1, na.rm=TRUE),
            #rmssd.DestructiveCoping = rmssd(DestructiveCoping, lag=1, na.rm=TRUE)
            #rmssd.AnyCoping = rmssd(AnyCoping, lag=1, na.rm=TRUE),
            #rmssd.AnyCopingAndHelpful = rmssd(AnyCopingAndHelpful, lag=1, na.rm=TRUE)
  ) %>% as.data.frame(.)

variables.CumWeek4 <- left_join(variables.CumWeek4, response.df, by = c("ID"))

write.csv(variables.CumWeek4, "Week4CumulativeData.csv", row.names = FALSE)

corr.CumWeek4 <- variables.CumWeek4 %>% select(-ID) %>% cor(., use = "pairwise.complete.obs")

write.csv(corr.CumWeek4,"corr.CumWeek4.csv")
