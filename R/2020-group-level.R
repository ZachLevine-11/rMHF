#' @import dplyr
#' @import anytime
#' @export
#'
#'
read_group_data <- function(fn = "2020/2020Data.csv"){
  df <- read.csv(system.file(fn, package = "RMHF"), stringsAsFactors = FALSE)
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
  df <- data.frame(df, stringsAsFactors = FALSE)
  #Add a column to keep track of the year.
  df <- dplyr::bind_cols("Year" = c(NA, rep("2019", 11), rep("2020", 11)), df)
  #Name everything properly.
  colnames(df) <- c("Year", "Date", namesToUse[2:length(namesToUse)])
  #Drop the first row containing leveled versions the names we no longer need.
  df <- df[2:nrow(df),]
  #And we're done.
  return(df)
}

#ARIMA diagnostics, useful for when we want to lag something that's not a time series.
#Create a data frame with one column a variable of interest and the second being that variable lagged n times.
#'@export
create_lag <- function(df = read_data(), var = "Met-minutes", lag_ = 1){
  df <- data.frame(read_data()[,var], "lag" =  rbind(rep(NA, lag_), read_data()[1:22-lag_,var]), stringsAsFactors = FALSE)
  df[,1] <- as.numeric(df[,1])
  df[,2] <- as.numeric(df[,2])
  colnames(df) <- c(var, "lag")
  return(df)
}

#' @import ggplot2
#' @import dplyr
#' @import anytime
#' @export
#'
#'
plot_attendance_both <- function(){
  df  <- read_data()
  df$att <- as.numeric(df$"% of patients who were no-shows")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  library(ggplot2)
  #PLotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = att, colour = year)) + geom_point() + labs(x = "Weeks", y = "% of appointments missed", title = "No-show rates")
  p <- p + geom_smooth(data = df[1:11,]) + geom_smooth(data = df[12:22,])
  p
}

#' @import ggplot2
#' @import dplyr
#' @import anytime
#' @export
#'
#'
plot_mets_both <- function(){
  df  <- read_data()
  df$mets <- as.numeric(df$"Met-minutes")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  #PLotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = mets, colour = year)) + geom_point() + labs(x = "Weeks", y = "Weekly Average", title = "Reported Met Minutes")
  p <- p + geom_smooth(data = df[1:11,]) + geom_smooth(data = df[12:22,])
  p
}

