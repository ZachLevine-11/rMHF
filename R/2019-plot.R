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
