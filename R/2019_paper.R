#Iterate over the Met value and return TRUE if there is a dash in the string - indicating that we are dealing with a range
check_ifRange <- function(metValue){
  rangeCounter <- 1
  while (rangeCounter <= str_length(metValue)){
    letter <- substr(metValue, start = rangeCounter, stop = rangeCounter)
    if (letter == "-"){
        #Make sure that a single value followed by a "met-mins" unit is not interpreted as a range by checking the class of characters around the dash. If the characters can't be converted to integers, then the dash is part of a "met-min" tag and should be treated as a value.
        theDashIndex <- gregexpr(pattern = "-", metValue)[[1]][1]
        letterBefore <- substr(metValue, start = theDashIndex - 1, stop = theDashIndex - 1)
        letterAfter <- substr(metValue, start = theDashIndex + 1, stop = theDashIndex + 1)
#Add a secondary check, because on a few occasions, the range values are put in with a space around the dash. Let's make sure those ranges aren't interpreted as values.
        twoLettersBefore <- substr(metValue, start = theDashIndex - 2, stop = theDashIndex - 2)
        twoLettersAfter <- substr(metValue, start = theDashIndex + 2, stop = theDashIndex + 2)
        primaryCheck <- !is.na(as.integer(letterBefore)) & !is.na(as.integer(letterAfter))
        secondaryCheck <- !is.na(as.integer(twoLettersBefore)) & !is.na(as.integer(twoLettersAfter))
        #If either the characters immediately around the dash are an integer, or the values two characters away are integers (can be converted to an integer without producing a NA value), this value is a range, and we should treat it accordingly.
        IsaTrueRange <- primaryCheck | secondaryCheck
        if (IsaTrueRange){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
    }
    else{
      rangeCounter <- rangeCounter + 1
    }
  }
  return(FALSE)
}

#Extract the range from a MET range column, and return it as an integer atomic vector of the form c(upper, lower).
extractRange <- function(metValue){
  dashIndex <- gregexpr(pattern = "-", metValue)[[1]][1]
  rangeLower <- substr(metValue, start = 1, stop = dashIndex - 1)
  rangeUpper <- substr(metValue, start = dashIndex + 1, stop = str_length(metValue))
  #The upper range will include the "Met" unit we do not want, so let's get rid of that part.
  rangeUpper <- justValue(rangeUpper)
  #Just to be safe, we'll call justValue on the lower range too.
  rangeLower <- justValue(rangeLower)
  return(c(rangeLower, rangeUpper))
}

#Remove the "MET" unit from data if it is just a value. First create a new string without the "MET" unit, and then convert that string to an integer.
justValue <- function(metValue){
  justValueCounter <- 1
  newValue <- ""
  while (justValueCounter <= str_length(metValue)){
    currentLetter <- substr(metValue, start = justValueCounter, stop = justValueCounter)
    #If the current letter is an integer, add it to the newValue string
    if (!is.na(as.integer(currentLetter))){
      newValue <- paste(newValue,  currentLetter, sep = "")
    }
    justValueCounter <- justValueCounter + 1
  }
  newValue <- as.integer(newValue)
  return(newValue)
}

#Return an integer vector for weeks of follow up for each appointment a patient has.
makeWeeksofFollowUpVector <- function(datesTimesDataFrame){
#First convert the dates to R date objects. This will make our lives a lot easier.
  counter <- 1
  nApts <- nrow(datesTimesDataFrame)
  datesVector <- rep(0, nApts)
  while (counter <= nrow(datesTimesDataFrame)){
    datesVector[[counter]] <- paste(datesTimesDataFrame[[counter, "Appointment.Date"]], datesTimesDataFrame[[counter, "Appointment.Time"]], sep = " ")
    counter <- counter + 1
  }
  datesVector <- lubridate::ymd_hm(datesVector)
  elapsedDays <- rep(0, nApts)
  counter <- 1
  intakeDate <- datesVector[[1]]
  while (counter <= nApts){
    currentApt <- datesVector[[counter]]
    elapsedDays[[counter]] <- as.duration(intakeDate %--% currentApt)/86400
    counter <- counter + 1
  }
  return(elapsedDays)
}

#Add a dash separator into the time column of the dataset to make it amenable to date analysis.
addTimeSep <- function(aptDataFrame){
  times <- aptDataFrame[,"Appointment.Time"]
  newTimes <- rep(0, length(times))
  counter <- 1
  nApts <- length(times)
  while (counter <= nApts){
    originalTime <- times[[counter]]
    if (!is.na(originalTime)){
      if (str_length(originalTime) == 3){
  #The hour is a single digit number, so format the time accordingly.
        hour <- substr(originalTime, start = 1, stop = 1)
        minutes <- substr(originalTime, start = 2, stop = 3)
      }
      else{
  #The hour is a two digit number..
        hour <- substr(originalTime, start = 1, stop = 2)
        minutes <- substr(originalTime, start = 3, stop = 4)
      }
      betterTimeValue <- paste (hour, minutes, sep = ":")
      newTimes[[counter]] <- betterTimeValue
    }
    counter <- counter + 1
  }
  aptDataFrame[,"Appointment.Time"] <- newTimes
  return(aptDataFrame)
}
#Sort each person's appointment dataframe into ascending order by appointment, so that the intake date and elapsed weeks into follow-up can be reliable set and used.
#Return the ordered data.frame for each person.
orderPersonDf <- function(personDataFrame){
  #First set the person's appointment date stamps to the format R can use.
  personDataFrame$Appointment.Date <- lubridate::mdy(personDataFrame$Appointment.Date)
  #We can sort by day, ignoring time, because no person would have had two appointments on the same day.
  personDataFrameOrdered <- personDataFrame[order(personDataFrame$Appointment.Date),] #Index by the order of the dates across all columns
  return(personDataFrameOrdered)
}


#Create the master dataset, copying MET minute values over to the corresponding appointment and patient, and setting any non reported MET values as NA.
#Make sure to set the working directory to the directory containing the .csv files before you run any of these functions.
create_data <- function(){
  aptTypes <- read.csv("allApptsFinal.csv", stringsAsFactors = FALSE)
  justMets <- read.csv("metMinsFinal.csv", stringsAsFactors = FALSE)
  combinedData <- aptTypes
  combinedData <- combinedData[combinedData$Appointment.Type != "OTN No Show Subsequent" & combinedData$Appointment.Type != "Clinical Re-assessment",]
#Sort each person's appointment list into ascending order, so that the intake date and elapsed weeks into follow-up can be reliable assessed.
  justUniquePeople <- unique(combinedData$PHN)
  numPeople <- length(justUniquePeople)
  counter <- 1
  orderedDataFrame <- data.frame(stringsAsFactors = FALSE)
  while(counter <= numPeople){
    person <- justUniquePeople[[counter]]
    personIndices <- combinedData$PHN == person
    persondf <- combinedData[personIndices,]
    personDfOrdered <- orderPersonDf(persondf)
    #Replace each person's section of the dataframe with an ordered version of their appointments, sorted in from oldest to latest appointment.
    orderedDataFrame <- dplyr::bind_rows(orderedDataFrame, personDfOrdered)
    counter <- counter + 1
  }
  combinedData <- orderedDataFrame
  #Eliminate appointments scheduled for the future.
#Add a separator into the time column so we can read it properly.
  combinedData <- addTimeSep(combinedData)
#First, add to each appointment the number of weeks of follow up it is at, setting intake to be at time = 0.
#We'll grab unique people by Personal Health Number, PHN, so as to avoid the problems of multiple people sharing the same first or last name.
  personCounter <- 1
  combinedData$WeeksofFollowUp <- rep(0, nrow(combinedData))
  while (personCounter <= numPeople){
    personPHN <- justUniquePeople[personCounter]
    personAppointmentIndices <- combinedData$PHN == personPHN
    personAppointmentDates <- combinedData[personAppointmentIndices, c("Appointment.Date", "Appointment.Time")]
    weeksofFollowupVector <- makeWeeksofFollowUpVector(personAppointmentDates) / 7
    #Add these dates to the corresponding appointments in the big appointments list.
    combinedData[personAppointmentIndices, "WeeksofFollowUp"] <- weeksofFollowupVector
    personCounter <- personCounter + 1
  }
  combinedData$reportedMets <- rep("Not reported", nrow(combinedData))
  justUniquePeople <- unique(combinedData$PHN)
  nPeople <- length(justUniquePeople)
  tempData <- data.frame()
  for (counter in 1:nPeople){
#We'll do this at the individual level now.
    personPHN <- justUniquePeople[counter]
    personAppointmentIndices <- combinedData$PHN == personPHN
    persondf <- combinedData[personAppointmentIndices,]
    nApts <- nrow(persondf)
    for (aptCounter in 1:nApts){
      aptDate <- lubridate::ymd(persondf$Appointment.Date[aptCounter])
#For each appointment in combinedData, find a corresponding MET value in the seperate database we have for them.
      justPerson <- justMets[justMets$PHN == personPHN,] #Create a dataframe just for that person using the PHN as a unique identifier (rather than names).
      #For each appointment entry in combinedData, check if there is a reported MET minute value in the justMets dataframe, and grab it if so.
      appointmentReport <- justPerson[lubridate::mdy(justPerson$Observation.Date) == aptDate, "Value"]
      ##If there is no corresponding MET value for the appointment, add a "Not reported" value to the dataset.
      #With multiple values, the other values are always duplicates of the first. Grab the first value always, so this doesn't become an issue.
      persondf[[aptCounter, "reportedMets"]] <- appointmentReport[1]
    }
    tempData <- dplyr::bind_rows(persondf, tempData)
  }
  combinedData <- tempData
#Now cycle through the MET Minute data and make it numerical.
  metFixCounter <- 1
  combinedData$metIsaRange <- rep(NA, nrow(combinedData))
  combinedData$metRangeLower <- rep(NA, nrow(combinedData))
  combinedData$metRangeUpper <- rep(NA, nrow(combinedData))
  combinedData$metValue <- rep("Not reported", nrow(combinedData))
  while (metFixCounter <= nrow(combinedData)){
#Handle missing values first.
    if (is.na(combinedData[metFixCounter, "reportedMets"])){
      combinedData[metFixCounter, "metIsaRange"] <- NA
    }
#Only operate on rows that have a value for the reported Mets per week.
   else if (check_ifRange(combinedData[metFixCounter, "reportedMets"]) == TRUE){
      combinedData[metFixCounter,"metIsaRange"] <- TRUE
      range <- extractRange(combinedData[metFixCounter, "reportedMets"])
      combinedData[metFixCounter, "metRangeLower"] <- range[1]
      combinedData[metFixCounter, "metRangeUpper"] <-  range[2]
      combinedData[metFixCounter, "metRangeMean"] <- (range[1] + range[2]) / 2
     }
    else{
#If it is not a range, just put the data in as an integer value.
      combinedData[metFixCounter,"metIsaRange"] <- FALSE
      combinedData[metFixCounter, "metValue"] <- justValue(combinedData[metFixCounter, "reportedMets"])
     }
  metFixCounter <- metFixCounter + 1
  }
#Drop the original MET minute data column.
#Only return reported values based on the parameter passed to the function
  #Make these integer values
  combinedData$metValue <- as.integer(combinedData$metValue)
  combinedData$metRangeLower <- as.integer(combinedData$metRangeLower)
  combinedData$metRangeUpper <- as.integer(combinedData$metRangeUpper)
  #We don't need these fields
  combinedData <- combinedData[, -c(6, 7, 10, 12, 13, 14,  16)]
  combinedData <- combinedData[combinedData$Appointment.Type != "OTN No Show Subsequent",]
#This is an obvious outlier, so let's remove it.
  combinedData <- combinedData[combinedData$PHN != "4121945572YX",]
  return(combinedData)
}


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

#Draw a plot of each person's MET values
individual_plot <- function(df){
  plot(0,0,xlim = c(0, 5),ylim = c(0,2500),type = "n", ylab = "Reported MET Minutes", xlab = "Appointment number, 0 = intake")
  justUniquePeople <- unique(df$PHN)
  numPeople <- length(justUniquePeople)
  peopleCounter <- 1
  metValues <- c()
  weeksofFollowUpValues <- c()
  #Plot each person's MET values by appointments
  while (peopleCounter <= numPeople){
    personPHN <- justUniquePeople[peopleCounter]
    personAppointmentIndices <- df$PHN == personPHN
    personDataFrame <- df[personAppointmentIndices,]
    numApts <- nrow(personDataFrame)
    aptCounter <- 1
    #For each person, create one vector for their appointment numbers, and one vector for the corresponding reported met value.
    while (aptCounter <= numApts){
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
      metValues <- c(metValue, metValues)
      weeksofFollowUpValues <- c(personDataFrame[aptCounter, "WeeksofFollowUp"], weeksofFollowUpValues)
      aptCounter <- aptCounter + 1
    }
    peopleCounter <- peopleCounter + 1
  }
  df <- data.frame(metValues, weeksofFollowUpValues)
  ggplot(data = df, mapping = aes(x = weeksofFollowUpValues, y = metValues)) + geom_point() + lims(y = c(0, 2500))
}

#Run catch_missing on data first before plotting it - otherwise it won't work.
aggregate_plot <- function(df){
  df <- aggregate(df, 4)
  p <- ggplot(data = df, mapping = aes(x = aptMeanWeeksofFollowUpMap, y = aptMeanMetsMap)) +
    geom_point(colour = "black") +
    geom_text(aes(label = numAttendeesVector), vjust = -1) +
    lims(y = c(0, 2500)) +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x), se = FALSE, colour = "white") +
    labs(x = "Weeks of follow up", y = "Mean reported MET Minutes", title = "All patient reported MET minutes and weeks of follow up")
  print(p)
  return(p)
}


#Create and aggregate attendance data.
#Make sure you're in the patientData directory in the Desktop before you run this.
#Make sure the mmaxApts value matches that used in the function call to aggregate() if you're trying to superimpose the plots.
#Make sure you're not using an excluded visit version of the data, i.e. catch_missing(mode = "excludeVisit")
create_aggregate_attendance <- function(maxApts = 4){
  df <- read.csv("attendance.csv", stringsAsFactors = FALSE)
  justUniquePeople <- unique(df$PHN)
  numPeople <- length(justUniquePeople)
  Nones <- c()
  Nzeroes <- c()
  for (i in 1:maxApts){
    Nones <- c(Nones, 0)
    Nzeroes <- c(Nzeroes, 0)
  }
#Use a hash table that stores the number of 1's and 0's as a tuple in the form the form c(num0's, num1's) for the ith appointment in the ith index of aptMap.
  for (i in 1:numPeople){
#i counts the person.
    personPHN <- justUniquePeople[i]
    personAppointmentIndices <- df$PHN == personPHN
    personDataFrame <- df[personAppointmentIndices,]
    numApts <- nrow(personDataFrame)
    indexToUse <- min(numApts, maxApts)
#To avoid indexing errors, only use the number of appointments if it's less than maxApts.
    for (j in 1:indexToUse){
#j counts the number of the appointment.
      if (personDataFrame[j, "Value"] == 0){
#Add one to the number of 0's for that appointment.
        Nzeroes[j] <- Nzeroes[j] + 1
      }
      else{
#Add one to the number of 1's for that appointment.
        Nones[j] <- Nones[j] + 1
      }
#Now the i-1th index of aptWeeksofFollowUpMap contains all the weeks of follow entries up that the ith appointment occured at. Let's grab the mean of it.
    }
  }
  aptNumbers <- (1:maxApts) - 1
  weeksofFollowUp <- aggregate(create_data())$aptMeanWeeksofFollowUpMap
  aggregate_df <- data.frame(aptNumbers, weeksofFollowUp, Nones, Nzeroes)
  return(invisible(aggregate_df))
}

#Calculate intake sample size, which is the number of intake appointmetns recorded as 1's in the attendance sheet
#Make sure you're in the patientData directory in the Desktop before you run this.
#Set df to to be the result of create_data(), which you should run before this to save time debugging.
get_total_intake_size <- function(df){
  all_df <- df
  attendance_df <- read.csv("attendance.csv", stringsAsFactors = FALSE)
  numIntakeOnes <- 0
  for (rowCounter in 1:nrow(attendance_df)){
    aptDate <- attendance_df[rowCounter, "Collection.Date"]
    attendance <- attendance_df[rowCounter, "Value"]
#The weeks of follow up is the weeks of follow up that the matching appointment in all_df (all the data) occured at.
    aptWeeksofFollowUp <- all_df[all_df$PHN == attendance_df[rowCounter, "PHN"] & lubridate:::ymd(all_df$Appointment.Date) == lubridate::mdy(aptDate), "WeeksofFollowUp"]
#If they did not attend, then there is no matching value. If they did attend, the length won't be zero.
    if (length(aptWeeksofFollowUp) != 0){
      if (aptWeeksofFollowUp == 0.00 &&  attendance == 1){
        numIntakeOnes <- numIntakeOnes + 1
      }
    }
  }
  return(invisible(numIntakeOnes))
}

#Plot the result of create_aggregate_attendance().
#Set useWeeksofFollowUp to TRUE to use weeks of follow-up on the x axis, and set it to false to use appointment numbers (0 for intake).
#Set holdConstant to TRUE to set the denominator for every appointment to be the intake appointment sample size.
plot_attendance <- function(maxApts = 4, useWeeksofFollowUp = TRUE, holdConstant = FALSE){
  df <- create_aggregate_attendance(maxApts)
  if (holdConstant){
    theData <- create_data()
    df$percent <- 100*df$Nones/sum(theData$WeeksofFollowUp == 0.00)
  }
  else{
    df$percent <- 100*df$Nones/(df$Nones + df$Nzeroes)
  }
  if (useWeeksofFollowUp){
    ggplot(data = df, mapping = aes(x = weeksofFollowUp, y = percent)) +
      geom_point(colour = "black") +
      lims(y = c(0, 100)) +
      labs(x = "Weeks of follow up (intake = 0)", y =  "Attendance (% of scheduled appointments attended)", title = "Program attendance") +
      geom_smooth(method = "lm", formula = y ~ splines::bs(x), se = FALSE, colour = "white")
  }
  else{
    ggplot(data = df, mapping = aes(x = aptNumbers, y = percent)) +
      geom_point(colour = "red") +
      lims(y = c(0, 100)) +
      labs(x = "Weeks of follow up (intake = 0)", y = "Attendance (% of scheduled appointments attended)", title = "Program attendance")
  }
}

#Statistics on age in the program
ageStats <- function(df){
  df$age <- lubridate::mdy("08-08-2019")- lubridate::mdy(df$Birthdate)
  ageData <- c(mean(df$age, na.rm = TRUE), sd(df$age, na.rm = TRUE))
  names(ageData) <- c("Mean Age", "Stdev")
  return(ageData)
}

#Output a csv file to the working directory with all columns in df, only selecting rows for which rowExpr evaluates to TRUE. Set name to the name of the file if you'd like to change it from the default value.
output_file <- function(df, rowExpr = is.na(theData$metIsaRange), name = "Missing reported mets.csv"){
  write.csv(df[rowExpr,], file = name)
}
