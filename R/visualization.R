#### Data Set Up ####
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

# test for differences assuming interaction term
bp_fm_III_age <- lm(BP ~ Treatment + Age + Treatment*Age, data = bp, 
                    contrasts = list(Treatment = contr.sum))
Anova(bp_fm_III_age, type = "III")


#### Two-Way ANOVA with sex as confounding variable ####
# mean and sd for bp
bp %>%
  group_by(Sex, Treatment) %>%
  summarise(mean = mean(BP),
            sd = sd(BP))


# looks like Medication A was the most succesful at lowering blood pressure, and females had lower results

# Fitting a two-way ANOVA model
sex_model <- aov(BP ~ Sex * Treatment,
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
#### Overall Impact of Treatment(s)####
treatment_effect <- aov(BP ~ Treatment, data = bp)

Anova(treatment_effect)

TukeyHSD(treatment_effect, conf.level = 0.95 )
#### ANCOVA with Age as Covariate ####

png(file = "ANCOVA_age.png",
    width = 1200, height = 800)

bp %>%
  ggplot(aes(x = Age, y = BP, color = Treatment))+
  geom_point()+
  geom_line(aes(y = predicted_fm_age))+
  labs(x = "Age", y = "Blood Pressure")+
  ggtitle("Efficacy of Treatment Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), legend.title = element_text(size = 14, face = "bold"))+
  scale_color_discrete(name = "Treatment Group", 
                       labels = c("Placebo", "Medication A", "Medication B"))

dev.off()

#### Two-Way ANOVA with sex as covariate ####
# Visualizing the differences in BP for each medication for both men and women
ggplot(bp, aes(x = Treatment, y = BP, fill = Sex)) +
  geom_violin(trim = FALSE)

# Adding the data points
png(file = "treatment_violin.png",
    width = 1200, height = 800)    

bp %>%
  ggplot(aes(x = Treatment, y = BP, fill = "#56B4E9")) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "white") + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', 
               position = position_dodge(1), binwidth = 0.5, 
               col = "white", alpha = 0.8) +
  stat_summary(
    fun.data = mean_cl_normal,   # calculates mean + 95% CI
    geom = "pointrange",         # point for mean, lines for CI
    size = 0.8,
    linewidth = 1.2,
    color = "black"
  ) +
  labs(x = "Treatment Group", y = "Blood Pressure") +
  scale_x_discrete(label = c("Placebo", "Medication A", "Medication B")) +
  ggtitle("Effect of Treatment on Blood Pressure") +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18, face = "bold"),
        legend.position = "none")

dev.off()
# Interaction Plot for each treatment group across Sex with 95% confidence intervals

png(file = "sex_effect_plot.png",
    width = 1200, height = 800)

bp %>%
  ggplot(aes(x = Treatment, y = BP, color = Sex, group = Sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)+
  scale_x_discrete(label = c("Placebo", "Medication A", "Medication B"))+
  labs(x = "Treatment Group", y = "Blood Pressure")+
  ggtitle("Effect of Sex on Treatment Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), legend.title = element_text(size = 14, face = "bold"))+
  scale_color_discrete(name = "Sex", labels = c("Female","Male"))

dev.off()