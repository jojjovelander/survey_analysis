library(stringr)
library(readr)
library(likert)
library(dbplyr)
library(stringi)
source("src/utils.R")

data <- loadCSVData('data/questionnaire_1.csv')
dataframe <- toDataFrame(data)

allDataDF <- toDataFrame(loadCSVData('data/myFileName.csv'))

valuesDF <- data.frame(allDataDF$Ownership, allDataDF$Management, 
                       allDataDF$Privact_Access, allDataDF$Privact_Control, 
                       allDataDF$Bias, allDataDF$Trust, allDataDF$Autonomy, 
                       allDataDF$Informed_Consent)

colHeadings = c("Ownership","Management", "Privacy_Access", "Privacy_Control", 
                "Bais", "Trust", "Autonomy", "Informed_Consent")

valuesDF <- renameColumns(valuesDF, colHeadings)
valuesDF <- fixMissingValues(valuesDF, "5")
valuesDF <- convertScaleToInt(valuesDF)
print(valuesDF)


