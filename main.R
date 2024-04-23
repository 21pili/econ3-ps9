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
library("stargazer")
library("tidyverse")
library("lmtest")

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
margins(probit_c)
stargazer(probit_c, type = "latex",
                    out = "OUTPUT/probit_c.tex",
                    label = "prbtc",
                    title = "Probit Regression")

### e

df <- df %>% mutate(pclass = as.numeric(pclass))

probit_e <- glm(survived ~ female + pclass, data = df, family = binomial(link = "probit")) #nolint
margins(probit_e)
stargazer(probit_e, type = "latex",
                    out = "OUTPUT/probit_e.tex",
                    label = "prbte",
                    title = "Probit Regression")

### f

df <- df %>% mutate(pclass = as.factor(pclass))
probit_f <- glm(survived ~ female + pclass, data = df, family = binomial(link = "probit")) #nolint
margins(probit_f)
stargazer(probit_f, type = "latex",
                    out = "OUTPUT/probit_f.tex",
                    label = "prbtf",
                    title = "Probit Regression")

### g

#How shoould I perform the LR_test ?
lr_test <- lrtest(probit_f, probit_c)
summary(lr_test)

### j

df <- df %>% mutate(pclass = as.numeric(pclass))

probit_j <- glm(survived ~ female + pclass + age, data = df, family = binomial(link = "probit")) #nolint
margins(probit_j)
stargazer(probit_j, type = "latex",
                    out = "OUTPUT/probit_j.tex",
                    label = "prbtj",
                    title = "Probit Regression")

# partial effect of age when 1st class, average age
avg_age <- mean(df$age, na.rm = TRUE)

