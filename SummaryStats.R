setwd("J:\\MI-SafeCope")
LongDataForAnalysis <- read.csv("LongDataForAnalysis.csv", header=TRUE)
OutcomeData <- subset(LongDataForAnalysis, Day==1, select=c(ID, m1_binaryoutcome))

dat <- read.csv("Merged Baseline and Followup File 01.24.17.csv", header=TRUE)
datesdf <- subset(dat, select = c(ï..ID, Discharge_Date, Date_1mo, m1_returnEDvisit_date)) 
colnames(datesdf) <- c("ID", "Discharge_Date", "Date_1mo", "m1_returnEDvisit_date")
OutcomeData <- merge(OutcomeData, datesdf, all.x=TRUE)

OutcomeData$Discharge_Date <- as.character(OutcomeData$Discharge_Date)
OutcomeData$Discharge_Date <- as.POSIXct(OutcomeData$Discharge_Date, format = "%m/%d/%Y")

OutcomeData$Date_1mo <- as.character(OutcomeData$Date_1mo)
OutcomeData$Date_1mo <- as.POSIXct(OutcomeData$Date_1mo, format = "%m/%d/%Y")

OutcomeData$m1_returnEDvisit_date <- as.character(OutcomeData$m1_returnEDvisit_date)
OutcomeData$m1_returnEDvisit_date <- as.POSIXct(OutcomeData$m1_returnEDvisit_date, format = "%m/%d/%Y")

OutcomeData$Day14 <- OutcomeData$Discharge_Date +14*24*60*60

mean(OutcomeData$m1_returnEDvisit_date-OutcomeData$Day14, na.rm=TRUE)
sd(OutcomeData$m1_returnEDvisit_date-OutcomeData$Day14, na.rm=TRUE)

CompleteOutcomeData <- subset(OutcomeData, !is.na(m1_binaryoutcome))


mean(CompleteOutcomeData$Date_1mo-CompleteOutcomeData$Day14, na.rm=TRUE)
sd(CompleteOutcomeData$Date_1mo-CompleteOutcomeData$Day14, na.rm=TRUE)
