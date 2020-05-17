#Split by those who had a MET minute counter < value and those who had a number >= value per week at baseline. Return the split dataframes in an array.
#Set singleDf to TRUE to return a single dataframe with a "Baseline" column. "Baseline" is composed of two values: "Exerciser" and "NonExerciser" at baseline, with each person named accordingly, rather than an array of two dataframes.
#Set values to the values you'd like to use for a split if singleDf = TRUE.
baseline_exercise_split <- function(dataFrame, value, plot = FALSE, singleDf = FALSE, values = c("Exerciser", "NonExerciser")){
  #Initialize the dataframes we're going to return.
  lowerDf <- data.frame()
  upperDf <- data.frame()
  newDf <- data.frame()
  justUniquePeople <- unique(dataFrame$PHN)
  numPeople <- length(justUniquePeople)
  personCounter <- 1
  while (personCounter <= numPeople){
    personPHN <- justUniquePeople[personCounter]
    personAptVectors <- dataFrame$PHN == personPHN
    internalCounter <- 1
    personDataFrame <- dataFrame[personAptVectors,]
    baselineMets <- personDataFrame[1, c("metIsaRange", "metRangeLower", "metRangeUpper","metValue")]
    if (baselineMets$metIsaRange == TRUE){
      upper <- baselineMets$metRangeUpper
      lower <- baselineMets$metRangeLower
      #The value for ranges is the mean value within that range.
      baselineMets <- (upper + lower)/ 2
    }
    else{
      baselineMets <- baselineMets[[1, "metValue"]]
    }
    if(baselineMets > value){
      if (singleDf){
        personDataFrame$Baseline <- rep(values[[1]], nrow(personDataFrame))
        newDf <- rbind(newDf, personDataFrame)
      }
      else{
        upperDf <- rbind(upperDf, personDataFrame)
      }
    }
    #BaselineMets <= value.
    else{
      if (singleDf){
        personDataFrame$Baseline <- rep(values[[2]], nrow(personDataFrame))
        newDf <- rbind(newDf, personDataFrame)
      }
      else{
        lowerDf <- rbind(lowerDf, personDataFrame)
      }
    }
    personCounter <- personCounter + 1
  }
  #Return as selected.
  if (singleDf){
    return(newDf)
  }
  else{
    dfList <- list(lowerDf, upperDf)
    if (plot){
      aggregate_plot(dfList[[1]])
      readline("Press enter to move on to the next plot")
      aggregate_plot(dfList[[2]])
    }
    return (dfList)
  }
}
