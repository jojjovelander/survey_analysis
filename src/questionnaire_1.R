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
library(gmodels)
library(ca)
# library(FSA)

data <- loadCSVData('data/questionnaire_1.csv')
dataframe <- toDataFrame(data)

allDataDF <- toDataFrame(loadCSVData('data/myFileName.csv'))
# print(allDataDF)

allDataUnisDF <- toDataFrame(loadCSVData('data/UnisQuestionnaire1.csv'))
print(allDataUnisDF)

newHeading <- c("Worried_DC", "Ownership", "Management", 
                "Privacy_Access", "Privacy_Control", "Bias", 
                "Trust", "Autonomy", "Informed.Consent", 
                "Worried_Access_Uni_Data", "Worried_Usage_Uni_Data")

oldHeading <- c("allDataUnisDF.Worried_DC", "allDataUnisDF.Ownership", "allDataUnisDF.Management", 
                "allDataUnisDF.Privacy_Access", "allDataUnisDF.Privacy_Control", "allDataUnisDF.Bias", 
                "allDataUnisDF.Trust", "allDataUnisDF.Autonomy", "allDataUnisDF.Informed.Consent", 
                "allDataUnisDF.Worried_Access_Uni_Data", "allDataUnisDF.Worried_Usage_Uni_Data")
    
test <- data.frame(allDataUnisDF$Worried_DC, allDataUnisDF$Ownership,
                   allDataUnisDF$Management, allDataUnisDF$Privacy_Access,
                   allDataUnisDF$Privacy_Control, allDataUnisDF$Bias, 
                   allDataUnisDF$Trust, allDataUnisDF$Autonomy, 
                   allDataUnisDF$Informed.Consent, allDataUnisDF$Worried_Access_Uni_Data,
                   allDataUnisDF$Worried_Usage_Uni_Data)

test <- renameSpecificColumns(test, oldHeading, newHeading)
test <- fixMissingValues(test, "5")
test <- convertScaleToInt(test)

allDataUnisDF <- replaceColumnData(allDataUnisDF, test)

allDataUnisScalesDF <- fixMissingValues(allDataUnisScalesDF, "5")
allDataUnisScalesDF <- convertScaleToInt(allDataUnisScalesDF)
print(allDataUnisScalesDF)

write.csv(allDataUnisDF, "data/newDFAllData.csv")

table(allDataUnisDF$University, allDataUnisDF$Read_TC)

contingencyUni_TandC <- xtabs(~allDataUnisDF$University + allDataUnisDF$Read_TC, data= allDataUnisDF)
xtabs(~allDataUnisDF$University + allDataUnisDF$Read_TC, data= allDataUnisDF)
# contingencyUni_TandCDF <- as.data.frame.matrix(contingencyUni_TandC)
# summary(contingencyUni_TandC)
crosstable_Uni_TC <- CrossTable(allDataUnisDF$University, allDataUnisDF$Read_TC, expected = TRUE, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

contingencyAccess_Usage <- xtabs(~allDataUnisDF$Worried_Access_Uni_Data + allDataUnisDF$Worried_Usage_Uni_Data, data = allDataUnisDF)
xtabs(~allDataUnisDF$Worried_Access_Uni_Data + allDataUnisDF$Worried_Usage_Uni_Data, data = allDataUnisDF)
crosstable_Access_Use <- CrossTable(allDataUnisDF$Worried_Access_Uni_Data, allDataUnisDF$Worried_Usage_Uni_Data, expected = TRUE, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
contingencyAccess_UsageDF <- as.data.frame.matrix(contingencyAccess_Usage)

valuesDF <- data.frame(allDataDF$Ownership, allDataDF$Management, 
                       allDataDF$Privact_Access, allDataDF$Privact_Control, 
                       allDataDF$Bias, allDataDF$Trust, allDataDF$Autonomy, 
                       allDataDF$Informed_Consent)

worriedDC <- data.frame(allDataDF$Worried_DC, allDataDF$Worried_Access_Uni_Data, allDataDF$Worried_Usage_Uni_Data)
worriedDC <- renameColumns(worriedDC, colHeadingsWorried)
worriedDC <- fixMissingValues(worriedDC, "5")
worriedDC <- convertScaleToInt(worriedDC)
print(worriedDC)
# summary(worriedDC)

worriedUni_TandC <- data.frame(allDataDF$Worried_Access_Uni_Data, allDataDF$Worried_Usage_Uni_Data, allDataDF$Read_TC)
colHeadingsworriedUni_TandC = c("Worried_Access_Uni_Data", "Worried_Usage_Uni_Data", "Read_TC")
worriedUni_TandC <- renameColumns(worriedUni_TandC, colHeadingsworriedUni_TandC)
worriedUni_TandC <- fixMissingValues(worriedUni_TandC, "5")
worriedUni_TandC <- convertScaleToInt(worriedUni_TandC$Worried_Access_Uni_Data, worriedUni_TandC$Worried_Usage_Uni_Data)

print(worriedUni_TandC)

worriedDC_TandC <- data.frame(allDataDF$Worried_DC, allDataDF$Read_TC)
colHeadingsWorriedDC_TandC = c("Worried_DC", "Read_TC")
worriedDC_TandC <- renameColumns(worriedDC_TandC, colHeadingsWorriedDC_TandC)
# print(worriedDC_TandC)

contingencyWorriedDC_TandC <- xtabs(~worriedDC_TandC$Worried_DC + worriedDC_TandC$Read_TC, data= worriedDC_TandC)
contingencyWorriedDC_TandCDF <- as.data.frame.matrix(contingencyWorriedDC_TandC)
print(contingencyWorriedDC_TandC)

# contingencyAccess_Usage <- xtabs(~worriedDC$Worried_Access_Uni_Data + worriedDC$Worried_Usage_Uni_Data, data = worriedDC)
# contingencyAccess_UsageDF <- as.data.frame.matrix(contingencyAccess_Usage)
# print(contingencyAccess_UsageDF)

worriedUsageUniData_TandC <- data.frame(allDataDF$Worried_Usage_Uni_Data, allDataDF$Read_TC)
colHeadingsWorriedUsageUniData_TandC = c("Worried_Usage_Uni_Data", "Read_TC")
worriedUsageUniData_TandC <- renameColumns(worriedUsageUniData_TandC, colHeadingsWorriedUsageUniData_TandC)
# print(worriedUsageUniData_TandC)

contingencyWorriedUsageUniData_TandC <- xtabs(~worriedUsageUniData_TandC$Worried_Usage_Uni_Data + worriedUsageUniData_TandC$Read_TC, data= worriedUsageUniData_TandC)
contingencyWorriedUsageUniData_TandC_DF <- as.data.frame.matrix(contingencyWorriedUsageUniData_TandC)
# print(contingencyWorriedUsageUniData_TandC)

contingencyWorriedDC_WorriedUniDC <- xtabs(~allDataDF$Aware_DC_by_uni + allDataDF$Worried_Usage_Uni_Data + allDataDF$Worried_Access_Uni_Data)
ftable(contingencyWorriedDC_WorriedUniDC) 


# mytable <- with(worriedDC, table(allDataDF$Aware_DC_by_uni, allDataDF$Worried_Usage_Uni_Data)) # create a 2 way table
# prop.table(mytable, 1) # row percentages
# prop.table(mytable, 2) # column percentages
# fit <- ca(mytable)
# print(fit) # basic results 
# summary(fit) # extended results 
# plot(fit) # symmetric map
# plot(fit, mass = TRUE, contrib = "absolute", map =
#        "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

crosstable_WorriedAccessUni_TC <- CrossTable(allDataDF$Worried_Access_Uni_Data, allDataDF$Read_TC, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
# crosstable_WorriedAccessUni_TC_DF <- as.data.frame.matrix(crosstable_WorriedAccessUni_TC)

psych::alpha(worriedDC)

cor.test(worriedDC$Worried_DC, worriedDC$Worried_Access_Uni_Data, method = "spearman")

cor.test(worriedDC_TandC$Worried_DC, worriedDC_TandC$Read_TC, method = "spearman")


colHeadings = c("Ownership","Management", "Privacy_Access", "Privacy_Control", 
                "Bias", "Trust", "Autonomy", "Informed_Consent")
colHeadingsWorried = c("Worried_DC", "Worried_Access_Uni_Data", "Worried_Usage_Uni_Data")

valuesDF <- renameColumns(valuesDF, colHeadings)
valuesDF <- fixMissingValues(valuesDF, "5")
valuesDF <- convertScaleToInt(valuesDF)
print(valuesDF)

# valuesDFRemoved <- removeValues(valuesDF, "removed")
# print(valuesDFRemoved)
# worriedDC <- renameColumns(worriedDC, colHeadingsWorried)
# print(worriedDC)

likertValuesDF <- convertToLikert(valuesDF, c("1", "2", "3", "4", "5"))

# test <- dataframe[dataframe[,heading] < 5, ]
#           
# likertValuesDF <- stripRowsByValue(likertValuesDF, 5)
# 
likertValuesDF <- likert(likertValuesDF)

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

