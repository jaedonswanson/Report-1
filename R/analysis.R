#### Loading Packages and Setting Up Data Frame ####
# Loading packages
library(tidyverse)
library(car)
library(emmeans)
# Creating the data frame that will be used for our analysis
bp <- read_csv("data.csv")
## changing the treatment and sex columns so R sees it as a categorical variable
bp$Treatment <- as.factor(bp$Treatment)
bp$Sex <- as.factor(bp$Sex)

#### ANOVA for Treatment Effect ####
# Fitting a two-way ANOVA model
sex_model <- aov(BP ~ Treatment, data = bp)
summary(sex_model)
## Assumption Check
plot(residuals(sex_model))
abline(0,0)
# Analyzing Treatment Differences
TukeyHSD(sex_model, conf.level = .95)
plot(TukeyHSD(sex_model, conf.level=.95), las = 2)


#### ANCOVA for Interaction(s) ####
# Fitting models to the data
bp_fm <- lm(BP ~ Treatment * Age * Sex, data = bp, 
            contrasts = list(Treatment = contr.sum, Sex = contr.sum))
## Assumption Checks
plot(residuals(bp_fm))
abline(0,0)
# adding the predictions to the data frame
bp$predicted <- predict(bp_fm)
# Testing for significance of interaction.
Anova(bp_fm, type = 3)