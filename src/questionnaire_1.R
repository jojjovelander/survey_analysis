library(stringr)
library(readr)
library(likert)
library(dbplyr)
library(stringi)
source("src/utils.R")
library(lattice)
require(mosaic)
# library(MASS)
# library(xtable)

data <- loadCSVData('data/questionnaire_1.csv')
dataframe <- toDataFrame(data)

allDataDF <- toDataFrame(loadCSVData('data/myFileName.csv'))

valuesDF <- data.frame(allDataDF$Ownership, allDataDF$Management, 
                       allDataDF$Privact_Access, allDataDF$Privact_Control, 
                       allDataDF$Bias, allDataDF$Trust, allDataDF$Autonomy, 
                       allDataDF$Informed_Consent)

worriedDC <- data.frame(allDataDF$Worried_DC, allDataDF$Worried_Access_Uni_Data, allDataDF$Worried_Usage_Uni_Data)
# print(worriedDC)

worriedDC_TandC <- data.frame(allDataDF$Worried_DC, allDataDF$Read_TC)
colHeadingsWorriedDC_TandC = c("Worried_DC", "Read_TC")
worriedDC_TandC <- renameColumns(worriedDC_TandC, colHeadingsWorriedDC_TandC)
print(worriedDC_TandC)

contingencyWorriedDC_TandC <- xtabs(~worriedDC_TandC$Worried_DC + worriedDC_TandC$Read_TC, data= worriedDC_TandC)
contingencyWorriedDC_TandCDF <- as.data.frame.matrix(contingencyWorriedDC_TandC)
print(contingencyWorriedDC_TandC)

worriedUsageUniData_TandC <- data.frame(allDataDF$Worried_Usage_Uni_Data, allDataDF$Read_TC)
colHeadingsWorriedUsageUniData_TandC = c("Worried_Usage_Uni_Data", "Read_TC")
worriedUsageUniData_TandC <- renameColumns(worriedUsageUniData_TandC, colHeadingsWorriedUsageUniData_TandC)
print(worriedUsageUniData_TandC)

contingencyWorriedUsageUniData_TandC <- xtabs(~worriedUsageUniData_TandC$Worried_Usage_Uni_Data + worriedUsageUniData_TandC$Read_TC, data= worriedUsageUniData_TandC)
contingencyWorriedUsageUniData_TandC_DF <- as.data.frame.matrix(contingencyWorriedUsageUniData_TandC)
print(contingencyWorriedUsageUniData_TandC)

colHeadings = c("Ownership","Management", "Privacy_Access", "Privacy_Control", 
                "Bias", "Trust", "Autonomy", "Informed_Consent")
colHeadingsWorried = c("Worried_DC", "Worried_Access_Uni_Data", "Worried_Usage_Uni_Data")

valuesDF <- renameColumns(valuesDF, colHeadings)
valuesDF <- fixMissingValues(valuesDF, "5")
valuesDF <- convertScaleToInt(valuesDF)
# valuesDFRemoved <- removeValues(valuesDF, "removed")
# print(valuesDFRemoved)
print(valuesDF)
# worriedDC <- renameColumns(worriedDC, colHeadingsWorried)
# print(worriedDC)

likertValuesDF <- convertToLikert(valuesDF, c("1", "2", "3", "4", "5"))

# test <- dataframe[dataframe[,heading] < 5, ]
#           
# likertValuesDF <- stripRowsByValue(likertValuesDF, 5)
# 
likertValuesDF <- likert(likertValuesDF)
print(likertValuesDF)

# plots summary with low, neutral, high mean and sd
# result = likert(likertValuesDF)
# summary(result)

plot(likertValuesDF)
# allDataDF$Worried_DC
# table(allDataDF$Worried_DC, allDataDF$Worried_Access_Uni_Data)
# table(worriedDC)
# contingencyWorriedAccess <- xtabs(~worriedDC$Worried_DC + worriedDC$Worried_Access_Uni_Data, data= worriedDC) 
# print(contingencyWorriedAccess)
# contingencyWorriedAccessDF <- as.data.frame.matrix(contingencyWorriedAccess)
# print(contingencyWOrriedAccessDF)
# with(worriedDC, table(worriedDC$Worried_DC, worriedDC$Worried_Access_Uni_Data))

# contingencyWorriedUsage <- xtabs(~worriedDC$Worried_DC + worriedDC$Worried_Usage_Uni_Data, data= worriedDC)
# print(contingencyWorriedUsage)
# contingencyWorriedUsageDF <- as.data.frame.matrix(contingencyWorriedUsage)

# cor.test(AllDataDF$How.worried.are.you.about.data.collection.of.personal.data.online.and.or.the.user.of.your.data., worriedDC$Worried_Usage_Uni_Data, data,
         # method = "spearman")

