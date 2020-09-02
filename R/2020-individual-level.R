#' @import dplyr
#' @import anytime
#' @import lubridate
#' @export
#'
#'
read_individual_data <- function(dir = "2020/", fn = "mets.csv"){
  df <- read.csv(system.file(paste0(dir, fn), package = "RMHF"), stringsAsFactors = FALSE)
  ##Properly type the date columns and add for age.
  df$Date <- anytime::anydate(df$Date)
  df$Birthday <- anytime::anydate(df$Birthday)
  df$Age <- lubridate::year(lubridate::today()) - lubridate::year(df$Birthday)
  return(df)
}
