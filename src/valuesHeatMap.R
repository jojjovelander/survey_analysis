library(stringr)
library(readr)
library(likert)
library(dbplyr)
library(stringi)
source("src/utils.R")
library(lattice)
require(mosaic)
library(gmodels)
library(ca)

allDataDF <- toDataFrame(loadCSVData('data/myFileName.csv'))

colHeadings = c("Ownership", "Management", "Privacy_Access", "Privacy_Control", 
                "Bias", "Trust", "Autonomy", "Informed_Consent")

valuesDF <- data.frame(allDataDF$Ownership, allDataDF$Management, 
                       allDataDF$Privact_Access, allDataDF$Privact_Control, 
                       allDataDF$Bias, allDataDF$Trust, allDataDF$Autonomy, 
                       allDataDF$Informed_Consent)

valuesDF <- renameColumns(valuesDF, colHeadings)

valuesDF <- fixMissingValues(valuesDF, "5")

valuesDF <- convertScaleToInt(valuesDF)

print(valuesDF)

# valuesDFRemoved <- removeValues(valuesDF, "removed")
# print(valuesDFRemoved)
# worriedDC <- renameColumns(worriedDC, colHeadingsWorried)
# print(worriedDC)

likertValuesDF <- convertToLikert(valuesDF, c("1", "2", "3", "4", "5"))

likertValuesDF <- stripRowsByValue(likertValuesDF, "5")

# test <- dataframe[dataframe[,heading] < 5, ]
#           

print(likertValuesDF)
# 
likertValuesDF <- likert(likertValuesDF)

# plots summary with low, neutral, high mean and sd
# result = likert(likertValuesDF)
# summary(result)

plot(likertValuesDF)

valuesUniDF <- data.frame(allDataUnisDF$University, allDataUnisDF$Ownership, allDataUnisDF$Management, allDataUnisDF$Privacy_Access,
                          allDataUnisDF$Privacy_Control, allDataUnisDF$Bias, allDataUnisDF$Trust, allDataUnisDF$Autonomy, allDataUnisDF$Informed.Consent)
print(valuesUniDF)
#valuesUniDF <- fixMissingValues(valuesUniDF, "5")
#valuesUniDF <- convertScaleToInt(valuesUniDF)

oldValuesUniDFHeading <- c("allDataUnisDF.University", "allDataUnisDF.Ownership", "allDataUnisDF.Management", "allDataUnisDF.Privacy_Access",
                           "allDataUnisDF.Privacy_Control", "allDataUnisDF.Bias", "allDataUnisDF.Trust", "allDataUnisDF.Autonomy", "allDataUnisDF.Informed.Consent")
newValuesUniDFHeading <-c("University", "Ownership", "Management", "Privacy_Access", "Privacy_Control", "Bias", "Trust", "Autonomy", "Informed.Consent")
valuesUniDF <- renameSpecificColumns(valuesUniDF, oldValuesUniDFHeading, newValuesUniDFHeading)

########## LNU ########

valuesLNU_DF <- filter(valuesUniDF, University == "LNU")
valuesLNU_DF

valuesLNU_DF$University <- NULL
likertValuesLNU_DF <- convertToLikert(valuesLNU_DF, c("1", "2", "3", "4", "5"))
likertValuesLNU_DF <- stripRowsByValue(likertValuesLNU_DF, "5")
print(likertValuesLNU_DF)

likertValuesLNU_DF <- likert(likertValuesLNU_DF)
plot(likertValuesLNU_DF)
likert.heat.plot(likertValuesLNU_DF)

########## SU ########

valuesSU_DF <- filter(valuesUniDF, University == "SU")
valuesSU_DF

valuesSU_DF$University <- NULL
likertValuesSU_DF <- convertToLikert(valuesSU_DF, c("1", "2", "3", "4", "5"))
likertValuesSU_DF <- stripRowsByValue(likertValuesSU_DF, "5")

likertValuesSU_DF <- likert(likertValuesSU_DF)
plot(likertValuesSU_DF)
likert.heat.plot(likertValuesSU_DF)
