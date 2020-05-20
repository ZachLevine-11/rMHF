#' @imports ggplot2
#' @imports dplyr
#' @imports anytime
read_data <- function(fn = "2020Data.csv"){
  df <- (read.csv(system.file(fn, package = "RMHF"), stringsAsFactors = FALSE))
  #Drop the empty rows:
  df <- df[c(1:11, 12:nrow(df)),]
  #Put 2020 in as a set of columns and order properly
  df <- dplyr::bind_cols(df[12:nrow(df),], df[1:10,])
  #Drop the extra labels column
  df <- df[,c(1:12, 14:length(df))]
  #Save the names here to use as column names
  namesToUse <- df[,1]
  #Take the transpose so we're dealing with rows of observation date.
  df <- t(df)
  #Add the date for the 2019 dates
  df[1:12,1] <- df[12:nrow(df),1]
  #Properly type the data.
  df <- data.frame(df)
  #Add a column to keep track of the year.
  df <- dplyr::bind_cols("Year" = c(NA, rep("2019", 11), rep("2020", 11)), df)
  #Name everything properly.
  colnames(df) <- c("Year", "Date", namesToUse[2:length(namesToUse)])
  #Drop the first row containing leveled versions the names we no longer need.
  df <- df[2:nrow(df),]
  #And we're done.
  return(df)
}

#Grab the entries around the week of March 16 for both years (2019, 2020) in question.
#df should be the result of read_data(), and it's set to that by default.
grab_march16 <- function(df = read_data()){
  dateEntries <- c("first" =  "March4-11", "second" = "March 12-19", "third" = "March20-27")
  grabbedWeeks <- df[df$Date %in% dateEntries,]
  return(grabbedWeeks)
}
