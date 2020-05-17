
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
