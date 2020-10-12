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
  df <- dplyr::distinct(df)
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



#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import anytime
#' @export
#'
#' @author Aaron Rosenfeld.
individual_mets_plot_aaron <- function(){
  df<- read_individual_data(fn = "mets_e.csv")
  df$Year <- lubridate::year(df$Date)
  df$Year <- as.character(df$Year)
  #Plotting 2020 only
  p <- ggplot(df, mapping = aes(x = Week, y = Mets, colour = Year)) + labs(x = "Weeks", y = "Reported MET-Minutes")
  p <- p + geom_smooth(data = df[1:563,], fill =  "#F8766D") + geom_smooth(data = df[564:914,], fill =  "#00BFC4") + geom_jitter(size =0.7) + geom_vline(xintercept=6, linetype="dotted")
  p <- p + theme_classic(base_size = 13, base_line_size = 1, base_rect_size = 1)
  p <- p + scale_y_continuous(limits = c(0, 4600), breaks = seq(0, 4600, 500))
  p <- p + scale_x_continuous(limits = c(1, 11), breaks = seq(1, 11, 1))
  p <- p + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face ='bold'), legend.position = 'top')
  p
}
