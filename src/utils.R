# if(!exists('utils_R')) {
#   
#   utils_R<-T
  
  loadCSVData <- function (path) {
    return (read.csv(path))
  }
  
  toDataFrame <- function (rawData) {
    return (data.frame(rawData))
  }

  renameSpecificColumns <- function (dataframe, oldColumnHeadings, newColumnHeadings) {
    if (length(oldColumnHeadings) != length(newColumnHeadings)) {
      errorCondition ("Old and new heading vectors must be the same length.")
    }
    for (j in seq_along(oldColumnHeadings)) {
      for (i in 1:ncol(dataframe)) {
        if (oldColumnHeadings[j] == names(dataframe)[i]) {
          names(dataframe)[i] <- newColumnHeadings[j] 
          break
        }
      }
    }
    return (dataframe)
  }
  
  replaceColumnData <- function (dataframe, newData) {
    for (j in 1:ncol(newData)) {
      for (i in 1:ncol(dataframe)) {
        if (names(newData)[j] == names(dataframe)[i]) {
          dataframe[i] <- newData[j]
          break
        }
      }
    }
    return (dataframe)
  }
  
  renameColumns <- function (dataframe, newColumnHeadings){
    for (i in 1:ncol(dataframe)) {
      names(dataframe)[i] <- newColumnHeadings[i]  
    }
    return (dataframe)
  }
  
  fixMissingValues <- function (dataframe, default) {
    headings <- colnames(dataframe)
    for (heading in headings) {
      currentColumn <- dataframe[heading] 
      currentColumn [currentColumn == ""] <- default
      dataframe[heading] <- currentColumn
    }
    return(dataframe)
  }
  
  
  stripRowsByValue <- function (dataframe, default) {
    headings <- colnames(dataframe)
    for (heading in headings) {
      
      dataframe <- dataframe[dataframe[heading] != default, ]
    }
    return(dataframe)
  }

  convertScaleToInt <- function (dataframe) {
    headings <- colnames(dataframe)
    for (heading in headings) {
      dataframe[heading] <- c(na.omit(as.numeric(unlist(strsplit(unlist(dataframe[heading]), "[^0-9]+")))))
    }
    return (dataframe)
  }
  
  
  
  # convertBinaryToInt <- function (dataframe) {
  #   headings <- colnames(dataframe)
  #   for (heading in headings) {
  #     if (dataframe[heading] == "Yes")
  #     dataframe[heading] <- 1
  #   }
  #   return (dataframe)
  # }
  
  convertDataToInt <- function (dataframe) {
    headings <- colnames(dataframe)
    for (heading in headings) {
      dataframe[heading]
    }
  }
  
  createFactors = function(data, likertScale) {
    return (factor(data,
           ordered = TRUE,
           levels = likertScale))
  }
  
  convertToLikert <- function(dataframe, likertScale) {
    likertDF = data.frame(dataframe) 
    likertDF[TRUE,] = NA
    headings <- colnames(dataframe)
    for (heading in headings) {
      likertDF[heading] <- createFactors(dataframe[[heading]], likertScale)
    }
    return (likertDF)
  }
  
  # newdata <- subset(valuesDFRemoved, Ownership != 5 , select=c(Ownership))
  # convertToLikert <- function(dataframe, likertScale) {
  #   likertDF = data.frame(dataframe) 
  #   likertDF[TRUE,] = NA
  #   headings <- colnames(dataframe)
  #   for (heading in headings) {
  #     likertDF[heading] <- createFactors(dataframe[[heading]], likertScale)
  #   }
  #   return (likertDF)
  # }

# }