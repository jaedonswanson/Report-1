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

#### ANCOVA with Age as Covariate ####
# Assumption Checks
## Normality using a shapiro-wilk test
bp %>%
  group_by(Age) %>%
  summarise(
    W         = shapiro.test(BP)$statistic,
    p.value   = shapiro.test(BP)$p.value,
    normal    = ifelse(shapiro.test(BP)$p.value > 0.05, "Yes", "No")
  ) %>%
  print()
### Placebo does not seem to be normally distributed, but the other two medication groups do
## Homogenity of Variance using a levene's test
levene_result <- leveneTest(BP ~ Medication, data = bp, center = median)
print(levene_result)
cat("Interpretation:", ifelse(levene_result$`Pr(>F)`[1] > 0.05,
                              "Variances are equal (assumption met).",
                              "Variances are NOT equal (assumption violated)."), "\n")
### Assumptions are met

# Fitting models to the data
## model lacking an interaction term
### ni = 'no interaction'
bp_ni_model_age <- lm(BP ~ Treatment + Age, data = bp)
#### adding the predictions to the data frame
bp$predicted_ni_age <- predict(bp_ni_model_age)

## full model (includes an interaction term)
### fm = 'full model'
bp_fm_age <- lm(BP ~ Treatment * Age, data = bp)
#### adding the predictions to the data frame
bp$predicted_fm_age <- predict(bp_fm_age)

## Testing if the full model is an improvement
anova(bp_ni_model_age, bp_fm_age)

# test for differences assuming no interaction term
## using 'Type III' sums of squares
bp_ni_III_age <- lm(BP ~ Treatment + Age, data = bp, 
                    contrasts = list(Treatment = contr.sum))
Anova(bp_ni_III_age, type = "III")

## using 'Type I' (sequential) sum of squares
bp_ni_I_age <- lm(BP ~ Treatment + Age, data = bp)
Anova(bp_ni_I_age)

# Residual plot from the linear model
plot(residuals(bp_ni_III_age) ~ fitted(bp_ni_I_age))
abline(0,0)






#### Two-Way ANOVA with sex as covariate ####
# mean and sd for bp
bp %>%
  group_by(Sex, Treatment) %>%
  summarise(mean = mean(BP),
            sd = sd(BP))


# looks like Medication A was the most succesful at lowering blood pressure, and females had lower results

# Fitting a two-way ANOVA model
i_dont_know_whatever <- aov(BP ~ Sex * Treatment,
                 data = bp)
summary(sex_model)
        
# Testing Assumptions
## Independence, yes
## Normality
plot(residuals(sex_model))
abline(0,0)
## Looks appx normal
## Equal variance using levene's test
leveneTest(BP ~ Sex * Treatment, data = bp)
## p > .05 so variances are not statistically diff

# Analyzing Treatment Differences
TukeyHSD(sex_model, conf.level = .95)

plot(TukeyHSD(sex_model, conf.level=.95), las = 2)












