#Internal functions, used to check for the existence of reported met values before or after a given appointment, given a person's data frame and an appointment index.
subsequentMetValue <- function(personDataFrame, aptIndex){
  if (aptIndex == nrow(personDataFrame)){
    return(FALSE)
  }
  #Remaining appointments to check are those within aptIndex to the end of the data frame.
  for (i in aptIndex:nrow(personDataFrame)){
    if (nrow(personDataFrame) == 0){
      return(FALSE)
    }
    reported <- personDataFrame[i, 10]
    if (!is.na(reported)){
      return(TRUE)
    }
  }
  return(FALSE)
}
previousMetValue <- function(personDataFrame, aptIndex){
  if (aptIndex == 1){
    return(FALSE)
  }
  #Remaining appointments to check are those within aptIndex to the begining of the data frame.
  for (i in seq(from = aptIndex, to = 1, by = -1)){
    reported <- personDataFrame[i, 10]
    if (!is.na(reported)){
      return(TRUE)
    }
  }
  return(FALSE)
}
findFirsttrue <- function(aptIndices){
  for (i in 1:length(aptIndices)){
    if (aptIndices[i] == TRUE){
      return(i)
    }
  }
}
#Set mode to "previous" for previous value search, and to subsequent" for subsequent value search.
findNextVal <- function(aptIndex, personDataFrame, mode){
  if (mode == "previous"){
    for (i in seq(from = aptIndex, to = 1, by = -1)){
      reported <- personDataFrame[i, 10]
      if (!is.na(reported)){
        if (reported){
          return(personDataFrame[i, 14])
        }
        else{
          return(personDataFrame[i, 13])
        }
      }
    }
  }
  else{
    for (i in 1:nrow(personDataFrame)){
      reported <- personDataFrame[i, 10]
      if (!is.na(reported)){
        if (reported){
          return(personDataFrame[i, 14])
        }
        else{
          return(personDataFrame[i, 13])
        }
      }
    }
  }
}

#Internal function, do not use on its own. Return true if all four columns of a 1XN dataframe are NA values, and false otherwise.
isMissing <- function(metData){
  for (i in 1:length(metData)){
    #If any of the entries are not missing values, return FALSE
    if (!is.na(metData[i])){
      return(FALSE)
    }
  }
  #If we've gotten here, every entry is a missing value. Return TRUE.
  return(TRUE)
}

#Deal with missing reported met values in the dataset, and return the new version of the dataset.
#Set mode to "excludeVisit" to delete any visits that don't have reported MET values associated with them from the dataset.
#Set mode to "subsequent" to set missing reported MET values to the reported met value at a subsequent appointment, and delete appointments from the dataset if there are no subsequent values to use (if the patient dropped out of the program, or if they never reported another MET value).
#Set mode to "previous" to set missing reported MET values to the reported met value at a previous appointment, and delete appointments from the dataset if there are no previous values to use (if the patient dropped out of the program, or if they never reported another MET value).
#Set mode to "globalZero" to globally set all missing reported MET values to 0.
catch_missing <- function(df, mode){
  if (mode == "excludeVisit"){
    theDf <- data.frame()
    counter <- 1
    while (counter <= nrow(df)){
      metData <- df[counter, c(10, 11, 12, 13, 14)]
      if (!isMissing(metData)){
        theDf <- dplyr::bind_rows(df[counter,], theDf)
      }
      counter <- counter + 1
    }
  }
  if (mode == "subsequent" || mode == "previous"){
    theDf <- df
    justUniquePeople <- unique(df$PHN)
    #To keep track of how many rows we've deleted and make sure we're putting MET values in the right place.
    numDeleted <- 0
    for (i in 1:length(justUniquePeople)){
      personPHN <- justUniquePeople[i]
      personAppointmentIndices <- df$PHN == personPHN
      personDataFrame <- df[personAppointmentIndices,]
      numApts <- nrow(personDataFrame)
      for (j in 1:nrow(df[df$PHN == justUniquePeople[i], ])){
        metData <- personDataFrame[j, c(10, 11, 12, 13, 14)]
        if (isMissing(metData)){
          #If met data is missing for an appointment, iterate over that person's remaining appointments and look for use the other reported MET values.
          if (mode == "previous"){
            #The appointment's index in the person's data frame in is the appointment index in the master data frame minus the index of the first appointment that person attended in the master data frame minus the number of appointments above it that have been deleted minus one (to avoid the off-by-one error).
            if (previousMetValue(personDataFrame, j)){
              #Grab the mean value for ranges, or the true value for values.
              theDf[[j + findFirsttrue(personAppointmentIndices) - numDeleted - 1 , 13]] <- findNextVal(j, personDataFrame, mode = "previous")
              theDf[[j + findFirsttrue(personAppointmentIndices) - numDeleted - 1, 10]] <- FALSE
            }
            else{
              #Drop the row
              theDf <- theDf[-(j + findFirsttrue(personAppointmentIndices) - numDeleted - 1 ),]
              numDeleted <- numDeleted + 1
            }
          }
          else{
            if (subsequentMetValue(personDataFrame, j)){
              theDf[[j + findFirsttrue(personAppointmentIndices) - numDeleted - 1, 13]] <- findNextVal(j, personDataFrame, mode = "subsequent")
              theDf[[j + findFirsttrue(personAppointmentIndices) - numDeleted - 1, 10]] <- FALSE
            }
            else{
              theDf <- theDf[-(j + findFirsttrue(personAppointmentIndices) - numDeleted - 1),]
              numDeleted <- numDeleted + 1
            }
          }
        }
      }
    }
  }
  if (mode == "globalZero"){
    counter <- 1
    while (counter <= nrow(df)){
      metData <- df[counter, c(10, 11, 12, 13, 14)]
      if (isMissing(metData)){
        df[[counter, 13]] <- 0
        df[[counter, 10]] <- FALSE
      }
      counter <- counter + 1
    }
    theDf <- df
  }
  return(theDf)
}

