#### Data Set Up ####
# Loading packages
library(tidyverse)
library(car)
library(emmeans)
# Creating the data frame that will be used for our analysis
bp <- read_csv("data.csv")
## changing the treatment and sex columns so R sees it as a categorical variable
bp$Treatment <- as.factor(bp$Treatment)
bp$Sex <- as.factor(bp$Sex)

#### Treatment Effect Violin ####
png(file = "treatment_violin.png",
    width = 1200, height = 800)

bp %>%
  ggplot(aes(x = Treatment, y = BP)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "white", fill = "#FF83FA") + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', 
               position = position_dodge(1), binwidth = 0.5, 
               col = "white", alpha = 0.8, fill = "#87CEFF") +
  stat_summary(
    fun.data = mean_cl_normal,   # calculates mean + 95% CI
    geom = "pointrange",         # point for mean, lines for CI
    size = 0.8,
    linewidth = 0.6,
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
#### Sex Effect on Treatment ####
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
#### Blood Pressure on Medication Across the years ####
# Fitting models to the data
bp_fm_age <- lm(BP ~ Treatment * Age, data = bp)
## adding the predictions to the data frame
bp$predicted_fm_age <- predict(bp_fm_age)
pred <- predict(bp_fm_age, interval = "confidence", level = 0.95)
bp$predicted_fm_age <- pred[, "fit"]
bp$ci_lower <- pred[, "lwr"]
bp$ci_upper <- pred[, "upr"]
# Figure
png(file = "ANCOVA_age.png",
    width = 1200, height = 800)

bp %>%
  ggplot(aes(x = Age, y = BP, color = Treatment, fill = Treatment))+
  geom_point()+
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA)+
  geom_line(aes(y = predicted_fm_age))+
  labs(x = "Age", y = "Blood Pressure")+
  ggtitle("Efficacy of Treatment Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), legend.title = element_text(size = 14, face = "bold"))+
  scale_color_manual(name = "Treatment Group", 
                     labels = c("Placebo", "Medication A", "Medication B"),
                     values = c("#648FFF", "#FFB000", "#DC267F"))+
  scale_fill_manual(name = "Treatment Group",
                    labels = c("Placebo", "Medication A", "Medication B"),
                    values = c("#648FFF", "#FFB000", "#DC267F"))

dev.off()