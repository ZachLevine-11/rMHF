#' @import ggplot2
#' @import dplyr
#' @import anytime
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

#' @import lubridate
#Run an additive time series analysis on read_data() specifically around the three weeks containing March 16.
#Change the column selector to be whatever column is in question.
mhf_ts <- function(data = read_data(), var = "Met-minutes"){
  ts2019 <- ts(data[1:11, var], frequency = 1, start = c(5), end = c(18))
  ts2020 <- ts(data[12:nrow(data), var], frequency = 1, start = c(6), end = c(19))
  return(list("2019" = ts2019, "2020" = ts2020))
}

#Identify the trend in a time series by moving average.
#Year selects the year in question.
#tslist should be a list of two time series, most usefully the result of timeSeries().
#' @import forecast
ma_ts <- function(year, tsList = mhf_ts()) {
  TS <- tsList[[year]]
  ma_ts <- forecast::ma(TS, order = 2)
  return(ma_ts)
}

#Grab the entries around the week of March 16 for a year (2019, 2020) in question for a time series.
march16_ts <- function(TS){
  #Return data matching the indices for the three weeks surrounding and including march 16.
  TS <- TS[5:7]
  return(TS)
}

#' @import forecast
#Identify the trend in a time series by least squares regresion using a simple linear model.
#Year selects the year in question.
#tslist should be a list of two time series, most usefully the result of timeSeries().
lm_ts <- function(year, tsList = mhf_ts()){
  TS <- tsList[[year]]
  fit <- forecast::tslm(formula = TS ~ trend )
  return(fit)
}
