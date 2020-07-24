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
valuesDF <- stripRowsByValue(valuesDF, 5)

likertValuesDF <- convertToLikert(valuesDF, c("1", "2", "3", "4"))
print(likertValuesDF)
likertValuesDF <- likert(likertValuesDF)
plot(likertValuesDF)

# plots summary with low, neutral, high mean and sd
# result = likert(likertValuesDF)
# summary(result)

########## UNIS ########
valuesUniDF <- data.frame(allDataUnisDF$University, allDataUnisDF$Ownership, allDataUnisDF$Management, allDataUnisDF$Privacy_Access,
                          allDataUnisDF$Privacy_Control, allDataUnisDF$Bias, allDataUnisDF$Trust, allDataUnisDF$Autonomy, allDataUnisDF$Informed.Consent)
print(valuesUniDF)

oldValuesUniDFHeading <- c("allDataUnisDF.University", "allDataUnisDF.Ownership", "allDataUnisDF.Management", "allDataUnisDF.Privacy_Access",
                           "allDataUnisDF.Privacy_Control", "allDataUnisDF.Bias", "allDataUnisDF.Trust", "allDataUnisDF.Autonomy", "allDataUnisDF.Informed.Consent")
newValuesUniDFHeading <-c("University", "Ownership", "Management", "Privacy_Access", "Privacy_Control", "Bias", "Trust", "Autonomy", "Informed.Consent")
valuesUniDF <- renameSpecificColumns(valuesUniDF, oldValuesUniDFHeading, newValuesUniDFHeading)

########## LNU ########

valuesLNU_DF <- filter(valuesUniDF, University == "LNU")
valuesLNU_DF

valuesLNU_DF$University <- NULL
valuesLNU_DF <- stripRowsByValue(valuesLNU_DF, 5)
likertValuesLNU_DF <- convertToLikert(valuesLNU_DF, c("1", "2", "3", "4"))
print(likertValuesLNU_DF)

likertValuesLNU_DF <- likert(likertValuesLNU_DF)
plot(likertValuesLNU_DF)
likert.bar.plot(likertValuesLNU_DF)

########## SU ########

valuesSU_DF <- filter(valuesUniDF, University == "SU")
valuesSU_DF

valuesSU_DF$University <- NULL
valuesSU_DF <- stripRowsByValue(valuesSU_DF, 5)
likertValuesSU_DF <- convertToLikert(valuesSU_DF, c("1", "2", "3", "4"))


likertValuesSU_DF <- likert(likertValuesSU_DF)
plot(likertValuesSU_DF)
likert.heat.plot(likertValuesSU_DF)
