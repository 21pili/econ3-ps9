### Problem Set 9

### INSTALLATION - To be run once and in the following order

#install.packages("prediction_0.3.14.tar", repos = NULL, type = "source")       #nolint
#install.packages("margins_0.3.26.1.tar.gz", repos = NULL, type = "source")     #nolint
#install.packages("Greg")                                                       #nolint

### Libraries
library("ggplot2")
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

### a ###

ols_a <- lm(survived ~ female, df)
stargazer(ols_a, type = "latex",
                    out = "OUTPUT/ols_a.tex",
                    label = "olsa",
                    title = "OLS")


### b ###

robust_se <- sqrt(diag(vcovHC(ols_a)))
conf_interval <- coef(ols_a)[2] + qt(c(0.025, 0.975), df = df.residual(ols_a)) * robust_se[2] #nolint

### c ###

probit_c <- glm(survived ~ female, data = df, family = binomial(link = "probit")) #nolint
margins(probit_c)
stargazer(probit_c, type = "latex",
                    out = "OUTPUT/probit_c.tex",
                    label = "prbtc",
                    title = "Probit Regression")

### e ###

df <- df %>% mutate(pclass = as.numeric(pclass))

probit_e <- glm(survived ~ female + pclass, data = df, family = binomial(link = "probit")) #nolint
margins(probit_e)
stargazer(probit_e, type = "latex",
                    out = "OUTPUT/probit_e.tex",
                    label = "prbte",
                    title = "Probit Regression")

### f ###

df <- df %>% mutate(pclass = as.factor(pclass))
probit_f <- glm(survived ~ female + pclass, data = df, family = binomial(link = "probit")) #nolint
margins(probit_f)
stargazer(probit_f, type = "latex",
                    out = "OUTPUT/probit_f.tex",
                    label = "prbtf",
                    title = "Probit Regression")

### g ###

lr_test_g <- lrtest(probit_f, probit_e)


### i ###

df <- df %>% mutate(pclass = as.numeric(pclass))

probit_i <- glm(survived ~ female + pclass + fare, data = df, family = binomial(link = "probit")) #nolint
margins(probit_i)
stargazer(probit_i, type = "latex",
                    out = "OUTPUT/probit_i.tex",
                    label = "prbti",
                    title = "Probit Regression")

# reestimate model (e) removing NAs for NAs in fare column
df_i <- df %>% filter(!is.na(fare))
probit_e_bis <- glm(survived ~ female + pclass, data = df_i, family = binomial(link = "probit")) #nolint


lr_test_i <- lrtest(probit_i, probit_e_bis)

### j ###

df <- df %>% mutate(pclass = as.numeric(pclass))

probit_j <- glm(survived ~ female + pclass + age, data = df, family = binomial(link = "probit")) #nolint
margins(probit_j)
stargazer(probit_j, type = "latex",
                    out = "OUTPUT/probit_j.tex",
                    label = "prbtj",
                    title = "Probit Regression")

# partial effect of age when 1st class, average age
avg_age <- mean(df$age, na.rm = TRUE)

# extract coefficients from the probit model
coefficients_j <- coef(probit_j)
alpha <- coefficients_j["(Intercept)"]
beta1 <- coefficients_j["female"]
beta2 <- coefficients_j["pclass"]
beta3 <- coefficients_j["age"]

# compute the partial effect of age
partial_effect <- beta3 * pnorm(alpha + beta1 + beta2 + beta3 * avg_age)

### k ###

predicted_probs <- predict(probit_j, type = "response")
# compute partial effect of age for each observation
partial_effects <- coefficients_j["age"] * predicted_probs

# average partial effect of age
average_partial_effect <- mean(partial_effects)

### l ###

df <- df %>%
        filter(!is.na(age)) %>%
        mutate(marginal_effect = partial_effects)

# Create a ggplot object with age and marginal effects
plot <- ggplot(df, aes(x = age,
                        y = marginal_effect,
                        linetype = factor(female),
                        color = factor(pclass))) +
  geom_line(size = 0.5) +
  labs(x = "Age", y = "Marginal Effect of Age") +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Male", "Female")) + #nolint
  guides(linetype = guide_legend(title = "Gender")) +
  scale_color_manual(values = c("#cf3030", "#3d3dcc", "#ff9239"), labels = c("1st class", "2nd class", "3rd class")) + #nolint
  guides(color = guide_legend(title = "Class"))

ggsave("OUTPUT/marginal_effect_age.png", plot, width = 8, height = 6, units = "in") #nolint

# Calculate sorted marginal effects
sorted_marginal_effects <- df %>%
  arrange(marginal_effect) %>%
  pull(marginal_effect)

# Plot sorted marginal effects
plot_sorted_marginal_effects <- ggplot() +
  geom_line(aes(
        x = 1:length(sorted_marginal_effects), #nolint
        y = sorted_marginal_effects), color = "blue") +
  labs(x = "Observation Index", y = "Sorted Marginal Effects")

# Save the plot with an appropriate name
ggsave("OUTPUT/sorted_marginal_effects.png", plot_sorted_marginal_effects, width = 8, height = 6, units = "in") #nolint
