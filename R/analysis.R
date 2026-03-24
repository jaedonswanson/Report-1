# Loading packages
library(tidyverse)
library(car)
library(emmeans)
# Creating the data frame that will be used for our analysis
bp <- read_csv("data.csv")
## changing the treatment column so R sees it as a categorical variable
bp$Treatment <- as.factor(bp$Treatment)

# Sex as a secondary variable
## Fitting models to the data
### model lacking an interaction term
#### ni = 'no interaction'
bp_ni_model_sex <- lm(BP ~ Treatment + Sex, data = bp)
##### adding the predictions to the data frame
bp$predicted_ni_sex <- predict(bp_ni_model_sex)


### full model (includes an interaction term)
#### fm = 'full model'
bp_fm_sex <- lm(BP ~ Treatment * Sex, data = bp)
##### adding the predictions to the data frame
bp$predicted_fm_sex <- predict(bp_fm_sex)

### Testing if the full model is an improvement
anova(bp_ni_model_sex, bp_fm_sex)


## test for differences assuming no interaction term
### using 'Type III' sums of squares
bp_ni_III_sex <- lm(BP ~ Treatment + Sex, data = bp, 
                contrasts = list(Sex = contr.sum))
Anova(bp_ni_III_sex, type = "III")

### using 'Type I' (sequential) sum of squares
bp_ni_I_sex <- lm(BP ~ Treatment + Sex, data = bp)
Anova(bp_ni_I_sex)

## Residual plot from the linear model
plot(residuals(bp_ni_III_sex) ~ fitted(bp_ni_I_sex))
abline(0,0)




# Age as a secondary variable
## Fitting models to the data
### model lacking an interaction term
#### ni = 'no interaction'
bp_ni_model_age <- lm(BP ~ Treatment + Age, data = bp)
##### adding the predictions to the data frame
bp$predicted_ni_age <- predict(bp_ni_model_age)


### full model (includes an interaction term)
#### fm = 'full model'
bp_fm_age <- lm(BP ~ Treatment * Age, data = bp)
##### adding the predictions to the data frame
bp$predicted_fm_age <- predict(bp_fm_age)

### Testing if the full model is an improvement
anova(bp_ni_model_age, bp_fm_age)


## test for differences assuming no interaction term
### using 'Type III' sums of squares
bp_ni_III_age <- lm(BP ~ Treatment + Age, data = bp, 
                contrasts = list(Treatment = contr.sum))
Anova(bp_ni_III_age, type = "III")

### using 'Type I' (sequential) sum of squares
bp_ni_I_age <- lm(BP ~ Treatment + Age, data = bp)
Anova(bp_ni_I_age)

## Residual plot from the linear model
plot(residuals(bp_ni_III_age) ~ fitted(bp_ni_I_age))
abline(0,0)













## ANCOVA
### Preliminary model to get residuals
bp_pre <- lm(BP ~ Treatment + Sex, data = bp)

### QQ plot for normality
plot(bp_pre, 2)
#### looks appx normal

### jitter plot of BP vs. Treatment by sex
bp %>%
  ggplot(aes(x = Treatment, y = BP, color = Sex)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
