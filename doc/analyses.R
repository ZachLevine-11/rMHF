## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width = 5, fig.height = 5---------------------------------------
df <- RMHF::read_data()
df2019mets <- df[1:11, "Met-minutes"]
tsdf2019mets <- ts(df2019mets)
# Is the 2020 time-series stationary?
df2020mets <- df[12:22, "Met-minutes"]
tsdf2020mets <- ts(df2020mets)
# Met-minutes in 2019
plot(ts(RMHF::read_data()[1:11,"Met-minutes"]))
#  Met minutes in 2020
plot(ts(RMHF::read_data()[12:22,"Met-minutes"]))

## ------------------------------------------------------------------------
df <- RMHF::read_data()
# Is the 2019 time-series stationary?
tseries::adf.test(tsdf2019mets, alternative = "stationary")
# Is the 2020 time-series stationary?
tseries::adf.test(tsdf2020mets, alternative = "stationary")

## ------------------------------------------------------------------------
# Differencing 2019 data and comparing the result.
arima(tsdf2019mets, order = c(0,0,0))
tsdf2019mets_diff1<- diff(tsdf2019mets, differences = 1)
tseries::adf.test(tsdf2019mets_diff1, alternative = "stationary")
arima(tsdf2019mets, order = c(0,1,0))

# Differencing 2020 data and comparing the result.
arima(tsdf2020mets, order = c(0,0,0))
tsdf2020mets_diff1 <- diff(tsdf2020mets, differences = 1)
tseries::adf.test(tsdf2020mets_diff1, alternative = "stationary")
arima(tsdf2020mets, order = c(0,1,0))

## ------------------------------------------------------------------------
tseries::kpss.test(tsdf2019mets)
tseries::kpss.test(tsdf2020mets)

## ----fig.height = 4, fig.width = 4---------------------------------------
acf(as.numeric(tsdf2019mets))
pacf(as.numeric(tsdf2019mets))

acf(as.numeric(tsdf2020mets))
pacf(as.numeric(tsdf2020mets))


## ------------------------------------------------------------------------
#Extract frequency from 2019 data.
forecast::findfrequency(tsdf2019mets)
#Extract frequency from 2020 data.
forecast::findfrequency(tsdf2020mets)

## ------------------------------------------------------------------------
testSeasonality <- function(listobj){
  possibleFreqs <- 2:5
  for (thefreq in possibleFreqs){
    print(paste0("Is seasonal at a frequency of: ", thefreq, " ?" ))
    print(seastests::isSeasonal(ts(listobj), freq = thefreq, test = "seasdum"))
  }
}
#Try 2019.
testSeasonality(df2019mets)
#Try 2020.
testSeasonality(df2020mets)

## ----fig.width = 5-------------------------------------------------------
#Fit both ARIMA models.
arima2019 <- forecast::Arima(tsdf2019mets)
arima2020 <- forecast::Arima(tsdf2020mets)


#Check 2019 ARIMA.
forecast::checkresiduals(arima2019)

#Check 2020 ARIMA.
forecast::checkresiduals(arima2020)

## ------------------------------------------------------------------------
df <- RMHF::read_data()
colnames(df) <- c(colnames(df)[1:10], "mets")
df$mets <- as.numeric(df$mets)
df$weeks <- c(1:11, 1:11)
#Create a dummy variable for the year.
df$year <- c(rep(0, 11), rep(1, 11)) 

## ------------------------------------------------------------------------
#Fit the regression.
fit <- lm(mets ~ weeks + weeks*year, data  = df)

## ------------------------------------------------------------------------
forecast::checkresiduals(fit)
plot(fit)

## ------------------------------------------------------------------------
anova(fit)
summary(fit)

## ----fig.width = 5, fig.height = 5---------------------------------------
df <- RMHF::read_data()
df2019vol <- df[1:11, "Prescheduled appointments" ]
ts2019vol <- ts(df2019vol)

df2020vol <- df[12:22, "Prescheduled appointments" ]
ts2020vol <- ts(df2020vol)

plot(ts2019vol)
plot(ts2020vol)

## ------------------------------------------------------------------------
tseries::kpss.test(ts2019vol)
tseries::kpss.test(ts2020vol)

## ----fig.height = 3, fig.width = 4---------------------------------------
acf(as.numeric(ts2019vol))
pacf(as.numeric(ts2019vol))

acf(as.numeric(ts2020vol))
pacf(as.numeric(ts2020vol))


## ------------------------------------------------------------------------
#Extract frequency from 2019 data.
forecast::findfrequency(ts2019vol)
#Extract frequency from 2020 data.
forecast::findfrequency(ts2020vol)

## ------------------------------------------------------------------------
#Try 2019.
testSeasonality(ts2019vol)
#Try 2020.
testSeasonality(ts2020vol)

## ------------------------------------------------------------------------
df <- RMHF::read_data()
colnames(df) <- c(colnames(df)[1:2], "vol", colnames(df)[4:length(colnames(df))])
df$vol <- as.numeric(df$vol)
df$weeks <- c(1:11, 1:11)
#Create a dummy variable for the year.
df$year <- c(rep(0, 11), rep(1, 11)) 
#Fit the regression.
fitvol <- lm(vol ~ weeks + weeks*year, data  = df)

## ------------------------------------------------------------------------
forecast::checkresiduals(fitvol)
anova(fit)
summary(fitvol)

## ----fig.height = 5, fig.width = 5---------------------------------------
autofit <- forecast::auto.arima(ts(RMHF::read_data()[12:22, "Met-minutes"]))

## ------------------------------------------------------------------------
theDf <- data.frame("y" = as.numeric(RMHF::read_data()[12:22,]$"Met-minutes"), "weeks" = 1:11,  "int" = c(rep(0,6), rep(1,5)), stringsAsFactors = FALSE)
fullLm <- lm(data = theDf, formula = y ~ weeks + weeks*int)
half <- lm(data = theDf, formula = y ~ weeks)

## ------------------------------------------------------------------------
lmtest::lrtest(fullLm, half)

## ----fig.height = 5, fig.width = 5---------------------------------------
autofit <- forecast::auto.arima(ts(RMHF::read_data()[12:22, "Prescheduled appointments"]))
forecast::checkresiduals(autofit)

## ------------------------------------------------------------------------
theDf <- data.frame("y" = as.numeric(RMHF::read_data()[12:22,]$"Prescheduled appointments"), "weeks" = 1:11,  "int" = c(rep(0,6), rep(1,5)), stringsAsFactors = FALSE)
fullLm <- lm(data = theDf, formula = y ~ weeks + weeks*int)
half <- lm(data = theDf, formula = y ~ weeks)

## ------------------------------------------------------------------------
lmtest::lrtest(fullLm, half)

## ----fig.height = 5, fig.width = 5---------------------------------------
autofit <- forecast::auto.arima(ts(RMHF::read_data()[12:22, "% of patients who were no-shows"]))
forecast::checkresiduals(autofit)

## ------------------------------------------------------------------------
theDf <- data.frame("y" = as.numeric(RMHF::read_data()[12:22,]$"% of patients who were no-shows"), "weeks" = 1:11,  "int" = c(rep(0,6), rep(1,5)), stringsAsFactors = FALSE)
fullLm <- lm(data = theDf, formula = y ~ weeks + weeks*int)
half <- lm(data = theDf, formula = y ~ weeks)

## ------------------------------------------------------------------------
lmtest::lrtest(fullLm, half)

## ------------------------------------------------------------------------
library(ggplot2)
#Create the data encoded with a dummy variable for the pre/post occurence intervention.
ourDf <- data.frame(attendance = as.numeric(RMHF::read_data()[12:22,]$"% of patients who were no-shows"), mets = as.numeric(RMHF::read_data()[12:22,]$"Met-minutes"), int = c(rep(0,6), rep(1,5)))
ggplot(data = ourDf, mapping = aes(attendance, mets)) + geom_point() + labs(x = "Attendance", y = "Met-minutes")

## ------------------------------------------------------------------------
fit <- lm(data = ourDf, formula = mets~ attendance + attendance*int)
print(fit)
plot(fit)
summary(fit)

## ------------------------------------------------------------------------
library(ggplot2)
#Create the data encoded with a dummy variable for the pre/post occurence intervention.
ourDf <- data.frame(percentfemales = as.numeric(RMHF::read_data()[12:22,]$"% Females"), meanage = as.numeric(RMHF::read_data()[12:22,]$"Age"), int = c(rep(0,6), rep(1,5)))
ggplot(data = ourDf, mapping = aes(percentfemales, meanage)) + geom_point() + labs(x = "% Females", y = "Mean age")

## ------------------------------------------------------------------------
fit <- lm(data = ourDf, formula = meanage ~ percentfemales + percentfemales*int)
print(fit)
plot(fit)
summary(fit)

