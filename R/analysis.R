# Loading the 'tidyverse' and 'car' packages
library(tidyverse)
library(car)

# reading the data to a variable
bp <- read_csv("data.csv")

# changing the treatment column so R sees it as a categorical variable
bp$Treatment <- as.factor(bp$Treatment)

#Fitting models to the data

## model lacking an interaction term
### ni = 'no interaction'

bp_ni_model <- lm(BP ~ Treatment + Sex, data = bp)

bp$predicted_ni <- predict(bp_ni_model)


## full model (includes an interaction term)
### fm = 'full model'
bp_fm <- lm(BP ~ Treatment * Sex, data = bp)

bp$predicted_fm <- predict(bp_fm)


# Test the improvement of the full model
anova(bp_ni_model, bp_fm)

# test for differences assuming no interaction term

## using 'Type III' sums of squares
bp_ni_III <- lm(BP ~ Treatment + Sex, data = bp, 
                contrasts = list(Sex = contr.sum))
str(bp_ni_III)

Anova(bp_ni_III, type = "III")

## using 'Type I' (sequential) sum of squares

bp_ni_I <- lm(BP ~ Treatment + Sex, data = bp)
Anova(bp_ni_I)

# Residual plot from the linear model
plot( residuals(bp_ni_III) ~ fitted(bp_ni_I) )
abline(0,0)

# normal Quantile plot of residuals
qqnorm(residuals(bp_ni_I), pch = 16, col = "firebrick", 
       las = 1, ylab = "Residuals", xlab = "Normal quantile", main = "")

