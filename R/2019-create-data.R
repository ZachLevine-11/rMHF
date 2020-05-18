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
  return(combinedData)
}
