## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
##Use to bypass having to use double colon operator each time.
read_data <- RMHF::read_data
df2019 <- read_data()[1:11,]
df2020 <- read_data()[12:22,]
covariate <- as.numeric(df2019$"% of patients who were no-shows")
var2020 <- as.numeric(df2020$"% of patients who were no-shows")
group <- c(rep(0,6), rep(1,5))
df <- data.frame("pretest" = covariate, "posttest" = var2020, "group" = group, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
library(rstatix)
anova_test(data = df, formula = posttest ~ pretest + group)

## ------------------------------------------------------------------------
var2019 <- as.numeric(df2019$"Met-minutes")
var2020 <- as.numeric(df2020$"Met-minutes")
group <- c(rep(0,6), rep(1,5))
df <- data.frame("pretest" = covariate, "posttest" = var2020, "group" = group, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
anova_test(data = df, formula = posttest ~ pretest + group)

## ------------------------------------------------------------------------
var2019 <- as.numeric(df2019$"% Females")
var2020 <- as.numeric(df2020$"% Females")
group <- c(rep(0,6), rep(1,5))
df <- data.frame("pretest" = covariate, "posttest" = var2020, "group" = group, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
anova_test(data = df, formula = posttest ~ pretest + group)

## ------------------------------------------------------------------------
var2019 <- as.numeric(df2019$"Age")
var2020 <- as.numeric(df2020$"Age")
group <- c(rep(0,6), rep(1,5))
df <- data.frame("pretest" = covariate, "posttest" = var2020, "group" = group, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
anova_test(data = df, formula = posttest ~ pretest + group)

## ------------------------------------------------------------------------
var2019 <- as.numeric(df2019$"Prescheduled appointments")
var2020 <- as.numeric(df2020$"Prescheduled appointments")
group <- c(rep(0,6), rep(1,5))
df <- data.frame("pretest" = covariate, "posttest" = var2020, "group" = group, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
anova_test(data = df, formula = posttest ~ pretest + group)

