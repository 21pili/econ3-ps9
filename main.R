### Problem Set 9

### INSTALLATION - To be run once and in the following order

#install.packages("prediction_0.3.14.tar", repos = NULL, type = "source")       #nolint
#install.packages("margins_0.3.26.1.tar.gz", repos = NULL, type = "source")     #nolint
#install.packages("Greg")                                                       #nolint

### Libraries

library("stats")
library("margins")
library("sandwich")
library("foreign")
library("Greg")

### Imports

df <- read.dta("titanic.dta")

### Core

### a

ols_a <- lm(survived ~ female, df)


