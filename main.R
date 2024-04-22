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
library("stargazer")

### Imports

df <- read.dta("titanic.dta")

### Core

### a

ols_a <- lm(survived ~ female, df)
stargazer(ols_a, type = "latex",
                    out = "OUTPUT/ols_a.tex",
                    label = "olsa",
                    title = "OLS")


### b

robust_se <- sqrt(diag(vcovHC(ols_a)))
conf_interval <- coef(ols_a)[2] + qt(c(0.025, 0.975), df = df.residual(ols_a)) * robust_se[2] #nolint

### c

probit_c <- glm(survived ~ female, data = df, family = binomial(link = "probit")) #nolint
stargazer(probit_c, type = "latex",
                    out = "OUTPUT/probit_c.tex",
                    label = "prbtc",
                    title = "Probit Regression")
