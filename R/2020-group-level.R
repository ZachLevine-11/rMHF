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
attendance_plot <- function(){
  df  <- read_group_data()
  df$att <- as.numeric(df$"% of patients who were no-shows")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  library(ggplot2)
  #Plotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = att, colour = year)) + geom_point() + labs(x = "Weeks", y = "% of appointments missed", title = "No-show rates")
  p <- p + geom_smooth(data = df[1:11,], fill =  "#F8766D" ) + geom_smooth(data = df[12:22,], fill ="#00BFC4") + ylim(0, 100) +  geom_vline(xintercept=6, linetype="dotted")
  p
}

#' @import ggplot2
#' @import dplyr
#' @import anytime
#' @export
#'
#'
mets_plot <- function(){
  df  <- read_group_data()
  df$mets <- as.numeric(df$"Met-minutes")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  #PLotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = mets, colour = year)) + geom_point() + labs(x = "Weeks", y = "Weekly Average", title = "Reported Met Minutes")
  p <- p + geom_smooth(data = df[1:11,], fill =  "#F8766D" ) + geom_smooth(data = df[12:22,], fill ="#00BFC4") + ylim(0, 1000)  + geom_vline(xintercept=6, linetype="dotted")
  p
}


#' @import ggplot2
#' @import dplyr
#' @import anytime
#' @import plotrix
#' @export
#'
#' @author Aaron Rosenfeld.
#'
attendance_plot_aaron <- function(){
  df  <- read_group_data()
  df$att <- as.numeric(df$"% of patients who were no-shows")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  library(ggplot2)
  #Plotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = att, colour = year)) + geom_point(size = 3) + labs(x = "Weeks", y = "Percentage of Appointments Missed")
  p <- p + geom_line(data = df[1:11,], fill =  "#F8766D") + geom_line(data = df[12:22,], fill ="#00BFC4") +  geom_vline(xintercept=6, linetype="dotted")
  p <- p + theme_classic(base_size = 13, base_line_size = 1, base_rect_size = 1)
  p <- p + scale_x_continuous(breaks = seq(0, 11, 1))
  p <- p + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%"))
  p <- p + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face ='bold'), legend.position = 'top')
  p
}

#' @import ggplot2
#' @import dplyr
#' @import anytime
#' @export
#'
#' @author Aaron Rosenfeld.
#'
mets_plot_aaron <- function(){
  df  <- read_group_data()
  df$mets <- as.numeric(df$"Met-minutes")
  df$weeks <- c(1:11, 1:11)
  df$year <- c(rep("2019", 11), rep("2020", 11))
  #PLotting 2020 only
  p <- ggplot(df, mapping = aes(x = weeks, y = mets, colour = year)) + geom_point(size = 3) + labs(x = "Weeks", y = "Weekly Average MET-Minutes")
  p <- p + geom_line(data = df[1:11,], fill =  "#F8766D" ) + geom_line(data = df[12:22,], fill ="#00BFC4") + geom_vline(xintercept=6, linetype="dotted")
  p <- p + theme_classic(base_size = 13, base_line_size = 1, base_rect_size = 1)
  p <- p + scale_x_continuous(breaks = seq(0, 11, 1))
  p <- p + scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, 100))
  p <- p + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face ='bold'), legend.position = 'top')
  p
}
