## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(lubridate)
library(forecast)
library(rstatix)

## ------------------------------------------------------------------------

##Change the year to select the year
splitbyyear <- function(df, year = "2019"){
  df[lubridate::year(df$Date) == year,]
}
mets2019 <- splitbyyear(RMHF::read_individual_data(fn = "mets.csv"), "2019")
mets2020 <- splitbyyear(RMHF::read_individual_data(fn = "mets.csv"), "2020")
##Order by date.
mets2019 <- mets2019[order(mets2019$Date),]
mets2020 <- mets2020[order(mets2020$Date),]

volume2019 <- splitbyyear(RMHF::read_individual_data(fn = "volume.csv"), "2019")
volume2020 <- splitbyyear(RMHF::read_individual_data(fn = "volume.csv"), "2020")
##Order by date.
volume2019 <- volume2019[order(volume2019$Date),]
volume2020 <- volume2020[order(volume2020$Date),]

## ------------------------------------------------------------------------
##Should do the heavy lifting for us.
aggregate_date <- function(date, df, colselect = "Mets"){
  dfdate <- df[df$Date == date, colselect]
  return(mean(dfdate))
}
##Vectorized, gives data at the daily level.
meanmets2020 <- sapply(unique(mets2020$Date), aggregate_date, df = mets2020)
meanmets2019 <- sapply(unique(mets2019$Date), aggregate_date, df = mets2019)
meanage2020 <- sapply(unique(volume2020$Date), aggregate_date, df = volume2020, colselect = "Age")
meanage2019 <- sapply(unique(volume2019$Date), aggregate_date, df = volume2019, colselect = "Age")

## ------------------------------------------------------------------------
library(forecast)
auto.arima(meanmets2019)
auto.arima(meanmets2020)
auto.arima(meanage2019)
auto.arima(meanage2020)

## ------------------------------------------------------------------------
##Normal stuff, t - test between data in different years.
library(rstatix)
t.test(meanmets2020, meanmets2019[1:length(meanmets2020)], paired = TRUE)
testdf <- data.frame("covidyear" = meanmets2020,
                     "covid" = c(rep(0,15), rep(1,17)))
anova_test(data = testdf, 
           formula = covidyear ~ covid)

## ------------------------------------------------------------------------
##Normal stuff, t - test between data in different years.
t.test(meanage2020, meanage2019[1:length(meanage2020)], paired = TRUE)

##Help find the cut point for the intervention.
age2020map <- data.frame("Age" = meanage2020, "Date" = unique(volume2020$Date), stringsAsFactors = FALSE)[order(lubridate::ymd(data.frame("Age" = meanage2020, "Date" = unique(volume2020$Date), stringsAsFactors = FALSE)$Date)),]
age2020map <- age2020map[order(age2020map$Date),]
testdf <- data.frame("covidyear" = meanage2020,
                     ##Hardcoded groups, don't set.
                     "covid" = c(rep(0,23), rep(1,31)))
anova_test(data = testdf, 
           formula = covidyear ~ covid)

## ------------------------------------------------------------------------
df <- RMHF::read_group_data()
df2019 <- as.numeric(df[df$Year == "2019",]$"Prescheduled appointments")
df2020 <- as.numeric(df[df$Year == "2020",]$"Prescheduled appointments")

t.test(df2019, df2020, paired = TRUE)
testdf <- data.frame("covidyear" = df2020,
                     "covid" = c(rep(0,6), rep(1,5)))
anova_test(data = testdf, 
           formula = covidyear ~ covid)

## ------------------------------------------------------------------------
df <- RMHF::read_group_data()
df2019 <- as.numeric(df[df$Year == "2019",]$"% of patients who were no-shows")
df2020 <- as.numeric(df[df$Year == "2020",]$"% of patients who were no-shows")

t.test(df2019, df2020, paired = TRUE)
testdf <- data.frame("covidyear" = df2020,
                     "covid" = c(rep(0,6), rep(1,5)))
anova_test(data = testdf, 
           formula = covidyear ~ covid)

## ------------------------------------------------------------------------
df <- RMHF::read_group_data()
df2019 <- as.numeric(df[df$Year == "2019",]$"% Females")
df2020 <- as.numeric(df[df$Year == "2020",]$"% Females")

t.test(df2019, df2020, paired = TRUE)
testdf <- data.frame("covidyear" = df2020,
                     "covid" = c(rep(0,6), rep(1,5)))
anova_test(data = testdf, 
           formula = covidyear ~ covid)

## ------------------------------------------------------------------------
mets2020 <- RMHF::read_group_data()[RMHF::read_group_data()$Year == "2020",]$"Met-minutes"
mets2019 <- RMHF::read_group_data()[RMHF::read_group_data()$Year == "2019",]$"Met-minutes"
att2020 <- RMHF::read_group_data()[RMHF::read_group_data()$Year == "2020",]$"% of patients who were no-shows"
att2019 <- RMHF::read_group_data()[RMHF::read_group_data()$Year == "2019",]$"% of patients who were no-shows"

#The ith element in meanmets is the mean of the mets collected on ith day in that year.
cor(as.numeric(mets2019), as.numeric(att2019))
cor(as.numeric(mets2020), as.numeric(att2020))

