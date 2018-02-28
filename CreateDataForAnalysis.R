# Set working directory to where raw data is saved
setwd("J:\\MI-SafeCope")

# The foreign package is needed because the raw data are in .sav format which is an SPSS file type
# We will read in the .sav files in R and perform the data cleaning and data analysis in R
# dplyr is an R package that will be used for data manipulation
# The inputs to this script are two files:
# Merged EMA Dataset_8.09.17_n34_partial.sav -- contains data on covariates
# Baseline and Follow Up Merged_10.02.17_partial.sav -- contains data on baseline and outcomes
# while the output of this script is a csv file of cleaned data in long format
library(foreign)
library(dplyr)

RawCovariateDataset <- read.spss("Merged EMA Dataset_8.09.17_n34_partial.sav", to.data.frame=TRUE)
RawOutcomeDataset <- read.spss("Baseline and Follow Up Merged_10.02.17_partial.sav", to.data.frame=TRUE)

# ToBinary and ToSumRows are helper functions which we will apply repeatedly later on
# ToBinary transforms a vector with two possible responses "Yes" and "No" to a 
# numeric vector of "1"'s and "0"'s
ToBinary <- function(OneColumn){
  OneColumn = as.character(OneColumn)
  OneColumn = ifelse(OneColumn=="Yes",1,0)
  OneColumn = as.numeric(OneColumn)
  return(OneColumn)
}

# ToSumRows was created because the sum() function with na.rm=TRUE returns zero when the list of inputs
# are all missing values. When a list contains all missing values, we want the sum of values in the list
# to be NA as well. However, when a list contains some missing values, but other values are not missing
# we want the sum over the list to be equal to the sum across non-missing values 
ToSumRows <- function(OneRow){
  if(sum(is.na(OneRow)) == length(OneRow)){
    total <- NA
  }else{
    total <- sum(OneRow,na.rm=TRUE)
  }
  return(total)
}


# We select the subset of columns we will use to construct features

Covariates <- RawCovariateDataset %>%
  select(ID, Day, Weekend, 
         Happy, Miserable, Angry, Hopelessness, Burdensomeness, Connectedness, Self_Efficacy, 
         SI, SI_Frequency, SI_Duration, SI_Urge, NSSI, SuicideAttempt,
         matches("SI_Coping"),matches("Feelings_Coping")) %>%
  select(-SI_Coping_Open,-Feelings_Coping_Open)


# The covariates below are measured at 28 consecutive days.
####### Non-suicide-related variables in the above are:
# Happy: "Please rate how much you felt this way in the past 24 hours . Happy"
# Miserable: "Please rate how much you felt this way in the past 24 hours . Miserable" 
# Angry: "Please rate how much you felt this way in the past 24 hours . Angry"
# Hopelessness: "I see only bad things ahead of me, not good things"
# Burdensomeness: "The people in my life would be happier without me"
# Connectedness: "I am close to other people"
####### Suicide-related variables in the above are:
# Self-efficacy: "How confident are you that you will be able to keep yourself from attempting suicide?"
# SI: At any point in the last 24 hours did you have any thoughts of killing yourself?
# SI_Urge: In the last 24 hours, how strong was the urge to act on your thoughts of suicide?
# NSSI: in the last 24 hours, did you harm yourself or your body without the intention to die
# Suicide_Attempt: At any point in the last 24 hours did you try to kill yourself or make yourself
# not alive anymore?

Outcomes <- RawOutcomeDataset %>%
  select(ID, Sex, Group, B_multipleattempt, B_attemptyesno,
         fm1_suicidalbeh, m1_rehospitalization, m1_returnEDvisit,
         outcome_returnEDvisit, outcome_attempt, outcome_behavior, outcome_rehospitalization)

# There are two sets of outcomes: The first set measures counts from day 1 to month 1 
# (name starts with 'fm1' or 'm1') while the second set measures counts from day 1 to month3 
# (name starts with 'outcome')
# fm1_suicidalbeh: Any actual, interrupted, or aborted suicide attempt
# m1_rehospitalization: Readmission to a hospital
# m1_returnEDvisit: Any visit to the emergency room for an evaluation
# outcome_returnEDvisit: Any visit  to emergency room for self-injury or suicide intent
# outcome_attempt: Any actual suicide attempt
# outcome_behavior: Any actual, interrupted, or aborted suicide attempt
# outcome_rehospitalization: Readmission to a hospital

# Clean up Outcomes data frame
# Variable: Sex
Outcomes <- Outcomes %>% mutate(Sex = as.character(Sex)) %>%
  mutate(Sex = replace(Sex, Sex == "Female", 1)) %>%
  mutate(Sex = replace(Sex, Sex == "Male", 0)) %>%
  mutate(Sex = as.numeric(Sex)) %>%
  rename(Female = Sex)

# Variable: Group
Outcomes <- Outcomes %>% mutate(Group = as.character(Group)) %>%
  mutate(Group = replace(Group, Group == "Intervention", 1)) %>%
  mutate(Group = replace(Group, Group == "Control", 0)) %>%
  rename(Intervention = Group)

# Variable: B_multipleattempt
Outcomes <- Outcomes %>% mutate(B_multipleattempt = as.character(B_multipleattempt)) %>%
  mutate(B_multipleattempt = replace(B_multipleattempt, B_multipleattempt == "Yes", 1)) %>%
  mutate(B_multipleattempt = replace(B_multipleattempt, B_multipleattempt == "No", 0)) 

# Variable: B_attemptyesno
Outcomes <- Outcomes %>% mutate(B_attemptyesno = as.character(B_attemptyesno)) %>%
  mutate(B_attemptyesno = replace(B_attemptyesno, B_attemptyesno == "Attempter", 1)) %>%
  mutate(B_attemptyesno = replace(B_attemptyesno, B_attemptyesno == "Non attempter", 0)) 

# Variable: fm1_suicidalbeh
Outcomes <- Outcomes %>% mutate(fm1_suicidalbeh = as.character(fm1_suicidalbeh)) %>%
  mutate(fm1_suicidalbeh = replace(fm1_suicidalbeh, fm1_suicidalbeh == "Yes", 1)) %>%
  mutate(fm1_suicidalbeh = replace(fm1_suicidalbeh, fm1_suicidalbeh == "No", 0)) %>%
  mutate(fm1_suicidalbeh = as.numeric(fm1_suicidalbeh))

# Variable: m1_rehospitalization
Outcomes <- Outcomes %>% mutate(m1_rehospitalization = as.character(m1_rehospitalization)) %>%
  mutate(m1_rehospitalization = replace(m1_rehospitalization, m1_rehospitalization == "Yes", 1)) %>%
  mutate(m1_rehospitalization = replace(m1_rehospitalization, m1_rehospitalization == "No", 0)) %>%
  mutate(m1_rehospitalization = replace(m1_rehospitalization, m1_rehospitalization == "0", 0)) %>%
  mutate(m1_rehospitalization = as.numeric(m1_rehospitalization))

# Variable: m1_returnEDvisit
Outcomes <- Outcomes %>% mutate(m1_returnEDvisit = as.character(m1_returnEDvisit)) %>%
  mutate(m1_returnEDvisit = replace(m1_returnEDvisit, m1_returnEDvisit == "Yes", 1)) %>%
  mutate(m1_returnEDvisit = replace(m1_returnEDvisit, m1_returnEDvisit == "No", 0)) %>%
  mutate(m1_returnEDvisit = replace(m1_returnEDvisit, m1_returnEDvisit == "0", 0)) %>%
  mutate(m1_returnEDvisit = as.numeric(m1_returnEDvisit))

# Variable: outcome_returnEDvisit
Outcomes <- Outcomes %>% mutate(outcome_returnEDvisit = as.character(outcome_returnEDvisit)) %>%
mutate(outcome_returnEDvisit = replace(outcome_returnEDvisit, outcome_returnEDvisit == "Yes", 1)) %>%
  mutate(outcome_returnEDvisit = replace(outcome_returnEDvisit, outcome_returnEDvisit == "No", 0)) %>%
  mutate(outcome_returnEDvisit = replace(outcome_returnEDvisit, outcome_returnEDvisit == "0", 0)) %>%
  mutate(outcome_returnEDvisit = as.numeric(outcome_returnEDvisit))

# Variable: outcome_attempt
Outcomes <- Outcomes %>% mutate(outcome_attempt = as.character(outcome_attempt)) %>%
  mutate(outcome_attempt = replace(outcome_attempt, outcome_attempt == "Yes", 1)) %>%
  mutate(outcome_attempt = replace(outcome_attempt, outcome_attempt == "No", 0)) %>%
  mutate(outcome_attempt = as.numeric(outcome_attempt))

# Variable: outcome_behavior
Outcomes <- Outcomes %>% mutate(outcome_behavior = as.character(outcome_behavior)) %>%
  mutate(outcome_behavior = replace(outcome_behavior, outcome_behavior == "Yes", 1)) %>%
  mutate(outcome_behavior = replace(outcome_behavior, outcome_behavior == "No", 0)) %>%
  mutate(outcome_behavior = as.numeric(outcome_behavior))  

# Variable: outcome_rehospitalization
Outcomes <- Outcomes %>% mutate(outcome_rehospitalization = as.character(outcome_rehospitalization)) %>%
  mutate(outcome_rehospitalization = replace(outcome_rehospitalization, outcome_rehospitalization == "Yes", 1)) %>%
  mutate(outcome_rehospitalization = replace(outcome_rehospitalization, outcome_rehospitalization == "No", 0)) %>%
  mutate(outcome_rehospitalization = replace(outcome_rehospitalization, outcome_rehospitalization == "0", 0)) %>%
  mutate(outcome_rehospitalization = as.numeric(outcome_rehospitalization)) 

# Create a new outcome variable for cumulative month 1 outcomes
Outcomes$m1_binaryoutcome <- apply(subset(Outcomes,select=c(m1_rehospitalization,m1_returnEDvisit,fm1_suicidalbeh)),1,ToSumRows)
Outcomes <- Outcomes %>% mutate(m1_binaryoutcome = replace(m1_binaryoutcome,is.na(m1_rehospitalization+m1_returnEDvisit+fm1_suicidalbeh) & (m1_binaryoutcome==0),NA))
Outcomes <- Outcomes %>% mutate(m1_binaryoutcome = replace(m1_binaryoutcome, m1_binaryoutcome>0,1))

# Create a new outcome variable for cumulative month 3 outcomes
Outcomes$m3cum_binaryoutcome <- apply(subset(Outcomes,select=c(outcome_rehospitalization,outcome_returnEDvisit,outcome_behavior)),1,ToSumRows)
Outcomes <- Outcomes %>% mutate(m3cum_binaryoutcome = replace(m3cum_binaryoutcome,is.na(outcome_rehospitalization+outcome_returnEDvisit+outcome_behavior) & (m3cum_binaryoutcome==0),NA))
Outcomes <- Outcomes %>% mutate(m3cum_binaryoutcome = replace(m3cum_binaryoutcome, m3cum_binaryoutcome>0,1))

# Clean up Covariates data frame
# Variable: Weekend
LongData <- Covariates %>% mutate(Weekend = as.character(Weekend)) %>%
  mutate(Weekend = replace(Weekend, Weekend == "Weekend",1)) %>%
  mutate(Weekend = replace(Weekend, Weekend == "Weekdays", 0)) %>%
  mutate(Weekend = as.numeric(Weekend))

# Variable: Week
LongData <- LongData %>% mutate(Week = if_else(Day <=7, 1,
                                               if_else((Day > 7) & (Day <= 14), 2,
                                                       if_else((Day > 14) & (Day <= 21),3,4))))

# Variable: Happy
LongData <- LongData %>% mutate(Happy = as.character(Happy)) %>%
  mutate(Happy = replace(Happy, Happy == "Very slightly/not at all (1)",1)) %>%
  mutate(Happy = replace(Happy, Happy == "A little (2)",2)) %>%
  mutate(Happy = replace(Happy, Happy == "Moderately   (3)", 3)) %>%
  mutate(Happy = replace(Happy, Happy == "Quite a bit  (4)",4)) %>%
  mutate(Happy = replace(Happy, Happy == "Extremely  (5)",5)) %>%
  mutate(Happy = as.numeric(Happy))

# Variable: Miserable
LongData <- LongData %>% mutate(Miserable = as.character(Miserable)) %>%
  mutate(Miserable = replace(Miserable, Miserable == "Very slightly/not at all (1)",1)) %>%
  mutate(Miserable = replace(Miserable, Miserable == "A little  (2)",2)) %>%
  mutate(Miserable = replace(Miserable, Miserable == "Moderately  (3)", 3)) %>%
  mutate(Miserable = replace(Miserable, Miserable == "Quite a bit  (4)",4)) %>%
  mutate(Miserable = replace(Miserable, Miserable == "Extremely  (5)",5)) %>%
  mutate(Miserable = as.numeric(Miserable))

# Variable: Angry
LongData <- LongData %>% mutate(Angry = as.character(Angry)) %>%
  mutate(Angry = replace(Angry, Angry == "Very slightly/not at all (1)",1)) %>%
  mutate(Angry = replace(Angry, Angry == "A little  (2)",2)) %>%
  mutate(Angry = replace(Angry, Angry == "Moderately  (3)", 3)) %>%
  mutate(Angry = replace(Angry, Angry == "Quite a bit  (4)",4)) %>%
  mutate(Angry = replace(Angry, Angry == "Extremely  (5)",5)) %>%
  mutate(Angry = as.numeric(Angry))

# Variable: Hopelessness
LongData <- LongData %>% mutate(Hopelessness = as.character(Hopelessness)) %>%
  mutate(Hopelessness = replace(Hopelessness, Hopelessness == "Strongly disagree", 1)) %>%
  mutate(Hopelessness = replace(Hopelessness, Hopelessness == "Disagree", 2)) %>%
  mutate(Hopelessness = replace(Hopelessness, Hopelessness == "Agree", 3)) %>%
  mutate(Hopelessness = replace(Hopelessness, Hopelessness == "Strongly agree", 4)) %>%
  mutate(Hopelessness = as.numeric(Hopelessness))

# Variable: Burdensomeness
# No need to clean up this variable as it is already in numeric format

# Variable: Connectedness
LongData <- LongData %>% mutate(Connectedness = as.character(Connectedness)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "Not at all true for me  (1)", 1)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "(2)", 2)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "(3)", 3)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "Some- what true for me  (4)", 4)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "(5)", 5)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "(6)", 6)) %>%
  mutate(Connectedness = replace(Connectedness, Connectedness == "Very true for me  (7)", 7)) %>%
  mutate(Connectedness = as.numeric(Connectedness))

# Variable: Self-Efficacy
LongData <- LongData %>% mutate(Self_Efficacy = as.numeric(as.character(Self_Efficacy)))

# Variable: SI
# No need to clean up this variable as it is already in numeric format

# Variable: SI_Urge
LongData <- LongData %>% mutate(SI_Urge = as.character(SI_Urge)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "Low  (1)", 1)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "(2)", 2)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "(3)", 3)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "(4)", 4)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "(5)", 5)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "(6)", 6)) %>%
  mutate(SI_Urge = replace(SI_Urge, SI_Urge == "High  (7)", 7)) %>%
  mutate(SI_Urge = as.numeric(SI_Urge))

# Variable: Urge_Combined
LongData <- LongData %>% mutate(Urge_Combined = SI_Urge)
LongData$Urge_Combined[is.na(LongData$Urge_Combined)] <- LongData$SI[is.na(LongData$Urge_Combined)]

# Drop the column SI_Urge since we have created the variable Urge_Combined
LongData <- LongData %>% select(-SI_Urge)

# Variable: SI_Frequency
LongData <- LongData %>% mutate(SI_Frequency = as.character(SI_Frequency)) %>%
  mutate(SI_Frequency = replace(SI_Frequency, SI_Frequency == "Only one time   (1)", 1)) %>%
  mutate(SI_Frequency = replace(SI_Frequency, SI_Frequency == "A few times  (2)", 2)) %>%
  mutate(SI_Frequency = replace(SI_Frequency, SI_Frequency == "A lot / Many times&nbsp;  (3)", 3)) %>%
  mutate(SI_Frequency = replace(SI_Frequency, SI_Frequency == "All the time  (4)", 4)) %>%
  mutate(SI_Frequency = as.numeric(SI_Frequency))

# Variable: Frequency_Combined
LongData <- LongData %>% mutate(Frequency_Combined = SI_Frequency)
LongData$Frequency_Combined[is.na(LongData$Frequency_Combined)] <- LongData$SI[is.na(LongData$Frequency_Combined)]

# Drop the column SI_Frequency since we have created the variable Frequency_Combined
LongData <- LongData %>% select(-SI_Frequency)

# Variable: SI_Duration
LongData <- LongData %>% mutate(SI_Duration = as.character(SI_Duration)) %>%
  mutate(SI_Duration = replace(SI_Duration, SI_Duration == "(1) A few seconds or minutes", 1)) %>%
  mutate(SI_Duration = replace(SI_Duration, SI_Duration == "(2) Less than 1 hour / some time", 2)) %>%
  mutate(SI_Duration = replace(SI_Duration, SI_Duration == "(3) 1-4 hours/ a lot of time", 3)) %>%
  mutate(SI_Duration = replace(SI_Duration, SI_Duration == "(4)\t4-8 hours / most of day", 4)) %>%
  mutate(SI_Duration = replace(SI_Duration, SI_Duration == "(5) More than 8 hours / continuous", 5)) %>%
  mutate(SI_Duration = as.numeric(SI_Duration))

# Variable: Duration_Combined
LongData <- LongData %>% mutate(Duration_Combined = SI_Duration)
LongData$Duration_Combined[is.na(LongData$Duration_Combined)] <- LongData$SI[is.na(LongData$Duration_Combined)]

# Drop the column SI_Frequency since we have created the variable Frequency_Combined
LongData <- LongData %>% select(-SI_Duration)

# Variable: NSSI
LongData <- LongData %>% mutate(NSSI = as.character(NSSI)) %>%
  mutate(NSSI = replace(NSSI, NSSI == "Yes", 1)) %>%
  mutate(NSSI = replace(NSSI, NSSI == "No", 0)) %>%
  mutate(NSSI = as.numeric(NSSI))

# Variable: SuicideAttempt
LongData <- LongData %>% mutate(SuicideAttempt = as.character(SuicideAttempt)) %>%
  mutate(SuicideAttempt = replace(SuicideAttempt, SuicideAttempt == "Yes", 1)) %>%
  mutate(SuicideAttempt = replace(SuicideAttempt, SuicideAttempt == "No", 0)) %>%
  mutate(SuicideAttempt = as.numeric(SuicideAttempt))

# Variable: Constructive Coping

tmpdat <- LongData %>% select(matches("SI_Coping_"), matches("Feelings_Coping_"), 
                                -SI_Coping_AlcoholDrugs, -SI_Coping_NSSI,
                                -Feelings_Coping_AlcoholDrugs, -Feelings_Coping_NSSI) %>% apply(.,2,ToBinary)

ConstructiveCoping <- apply(tmpdat,1,ToSumRows)

LongData <- LongData %>% mutate(ConstructiveCoping = ConstructiveCoping)

# Variable: Destructive Coping

tmpdat <- LongData %>% select(SI_Coping_AlcoholDrugs, SI_Coping_NSSI, 
                              Feelings_Coping_AlcoholDrugs, Feelings_Coping_NSSI) %>% apply(.,2,ToBinary)

DestructiveCoping <- apply(tmpdat,1,ToSumRows)

LongData <- LongData %>% mutate(DestructiveCoping = DestructiveCoping)

# Variable: Any Coping
tmpdat <- LongData %>% select(SI_Coping, Feelings_Coping) %>% apply(.,2,ToBinary)

AnyCoping <- apply(tmpdat,1,ToSumRows)

LongData <- LongData %>% mutate(AnyCoping = AnyCoping)

# Variable: Any Coping and coping helpful for those who had suicidal thoughts
LongData <- LongData %>% mutate(SI_Coping_HowHelpful = as.character(SI_Coping_HowHelpful)) %>%
  mutate(SI_Coping_HowHelpful = replace(SI_Coping_HowHelpful, SI_Coping_HowHelpful == "Not at all helpful  (1)", 1)) %>%
  mutate(SI_Coping_HowHelpful = replace(SI_Coping_HowHelpful, SI_Coping_HowHelpful == "(2)", 2)) %>%
  mutate(SI_Coping_HowHelpful = replace(SI_Coping_HowHelpful, SI_Coping_HowHelpful == "(3)", 3)) %>%
  mutate(SI_Coping_HowHelpful = replace(SI_Coping_HowHelpful, SI_Coping_HowHelpful == "(4)", 4)) %>%
  mutate(SI_Coping_HowHelpful = replace(SI_Coping_HowHelpful, SI_Coping_HowHelpful == "Extremely helpful  (5)", 5)) %>%
  mutate(SI_Coping_HowHelpful = as.numeric(SI_Coping_HowHelpful))

# Variable: Any Coping and coping helpful for those who did not have suicidal thoughts
LongData <- LongData %>% mutate(Feelings_Coping_HowHelpful = as.character(Feelings_Coping_HowHelpful)) %>%
  mutate(Feelings_Coping_HowHelpful = replace(Feelings_Coping_HowHelpful, Feelings_Coping_HowHelpful == "Not at all helpful  (1)", 1)) %>%
  mutate(Feelings_Coping_HowHelpful = replace(Feelings_Coping_HowHelpful, Feelings_Coping_HowHelpful == "(2)", 2)) %>%
  mutate(Feelings_Coping_HowHelpful = replace(Feelings_Coping_HowHelpful, Feelings_Coping_HowHelpful == "(3)", 3)) %>%
  mutate(Feelings_Coping_HowHelpful = replace(Feelings_Coping_HowHelpful, Feelings_Coping_HowHelpful == "(4)", 4)) %>%
  mutate(Feelings_Coping_HowHelpful = replace(Feelings_Coping_HowHelpful, Feelings_Coping_HowHelpful == "Extremely helpful  (5)", 5)) %>%
  mutate(Feelings_Coping_HowHelpful = as.numeric(Feelings_Coping_HowHelpful))

tmpdat <- LongData %>% select(SI_Coping_HowHelpful, Feelings_Coping_HowHelpful)
Coping_HowHelpful <- apply(tmpdat,1,ToSumRows)
LongData <- LongData %>% mutate(Coping_HowHelpful = Coping_HowHelpful)

LongData$AnyCopingAndHelpful <- NA
LongData <- LongData %>% 
  mutate(AnyCopingAndHelpful = if_else((AnyCoping==1) & (Coping_HowHelpful>=4),1,
                                       if_else((AnyCoping==1) & (Coping_HowHelpful<4),0,0)))
# Drop columns we do not need
LongData <- LongData %>% select(-matches("SI_Coping"), -matches("Feelings_Coping"))

# Complete LongData by joining the baseline and month 1 & 3 outcome
LongData <- left_join(LongData, Outcomes, by = c("ID"))

# Order data by Participant-Day
LongData <- LongData %>% arrange(ID, Day)
# The next line checks that each day has a corresponding line in the long dataset
#LongData %>% group_by(ID) %>% summarise(n()) %>% View(.)

write.csv(LongData, "LongDataForAnalysis.csv", row.names = FALSE)
