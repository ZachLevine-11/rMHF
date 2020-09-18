#' @import dplyr
#' @import anytime
#' @import lubridate
#' @export
#'
#'
read_individual_data <- function(fn = "mets.csv", dir = "2020/"){
  df <- read.csv(system.file(paste0(dir, fn), package = "RMHF"), stringsAsFactors = FALSE)
  ##Properly type the date columns and add one for age.
  df$Date <- anytime::anydate(df$Date)
  df$Birthday <- anytime::anydate(df$Birthday)
  df$Age <- lubridate::year(lubridate::today()) - lubridate::year(df$Birthday)
  return(df)
}


#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import anytime
#' @export
#'
#'
individual_mets_plot <- function(year = "2019"){
  df<- read_individual_data(fn = "mets.csv")
  df$Year <- lubridate::year(df$Date)
  df <- df[df$Year == year,]
  #Plotting 2020 only
  p <- ggplot(df, mapping = aes(x = Date, y = Mets)) + geom_point() + labs(x = "Weeks", y = "Reported mets", title = paste("Reported Met Minutes in ", year))
  p <- p + geom_smooth(data = df) + ylim(0, 1000)  + geom_vline(xintercept = lubridate::ymd(paste0(year, "/03/20")), linetype="dotted")
  p
}
