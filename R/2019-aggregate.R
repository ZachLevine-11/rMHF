
#' Aggregate data on a group level, and return group-level data we can plot from.
aggregate <- function(df, maxApts = 4){
  justUniquePeople <- unique(df$PHN)
  numPeople <- length(justUniquePeople)
  aptMap <- c()
  aptWeeksofFollowUpMap <- c()
  numAttendeesVector <- c()
  for (j in 1:maxApts){
    #Create two hash maps, one that contains the mean reported MET values for each appointment, and the other containing the weeks of follow up that each appointment happened at.
    aptMap <- c(aptMap, list(0))
    aptWeeksofFollowUpMap <- c(aptWeeksofFollowUpMap, list(0))
    numAttendeesVector <- c(numAttendeesVector, 0)
  }
  peopleCounter <- 1
  while (peopleCounter <= numPeople){
    personPHN <- justUniquePeople[peopleCounter]
    personAppointmentIndices <- df$PHN == personPHN
    personDataFrame <- df[personAppointmentIndices,]
    numApts <- nrow(personDataFrame)
    aptCounter <- 1
    #For each person, create one vector for their appointment numbers, and one vector for the corresponding reported met value.
    while (aptCounter <= numApts && aptCounter <= maxApts){
      #For each appointment grab the weeks of follow up that it occured.
      numAttendeesVector[[aptCounter]] <- numAttendeesVector[[aptCounter]] + 1
      weeksofFollowUp <- personDataFrame[aptCounter, "WeeksofFollowUp"]
      #If we're dealing with a range, grab the mean value in that range
      if (!is.na(personDataFrame[aptCounter, "metIsaRange"])){
        #If we're dealing with a range, plot the mean value in that range
        if (personDataFrame[aptCounter, "metIsaRange"]){
          metValue <- personDataFrame[aptCounter, "metRangeMean"]
        }
        #Otherwise, grab the single reported met value
        else{
          metValue <- personDataFrame[aptCounter, "metValue"]
        }
      }
      else{
        if (!is.na(personDataFrame[aptCounter, "metValue"])){
          metValue <- personDataFrame[aptCounter, "metValue"]
        }
      }
      #Only add the reported met minute value if we're within the range set by maxApts, and if the patient has a true intake appointment.
      if (aptCounter <= maxApts){
        aptMap[[aptCounter]] <- c(metValue, aptMap[[aptCounter]])
        aptWeeksofFollowUpMap[[aptCounter]] <- c(aptWeeksofFollowUpMap[[aptCounter]], weeksofFollowUp)
      }
      else{
      }
      aptCounter <- aptCounter + 1
    }
    peopleCounter <- peopleCounter + 1
  }
  aptMeanMetsMap <- c()
  aptMeanWeeksofFollowUpMap <- c()
  for (i in 1:length(aptMap)){
    aptSum <- 0
    weeksSum <- 0
    for (j in 1:length(aptMap[[i]])){
      if (!is.na(aptMap[[i]][[j]])){
        aptSum <- aptSum + aptMap[[i]][[j]]
        weeksSum <- weeksSum + aptWeeksofFollowUpMap[[i]][[j]]
      }
      else{
      }
      aptMeanMetsMap[[i]] <- aptSum/length(aptMap[[i]])
      aptMeanWeeksofFollowUpMap[[i]] <- weeksSum/length(aptWeeksofFollowUpMap[[i]])
    }
  }
  #Return the aggregated data in a dataframe.
  plotDf <- data.frame(aptMeanWeeksofFollowUpMap, aptMeanMetsMap, numAttendeesVector)
  return(invisible(plotDf))
}
