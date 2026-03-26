#### ANCOVA with Age as Covariate ####

png(file = "ANCOVA_age.png",
    width = 800, height = 800)

bp %>%
  ggplot(aes(x = Age, y = BP, color = Treatment))+
  geom_point()+
  geom_line(aes(y = predicted_ni_age, linetype = "solid"))+
  geom_line(aes(y = predicted_fm_age, linetype = "dotted"))+
  labs(x = "Age", y = "Blood Pressure")+
  ggtitle("Efficacy of Treatment Across Age Groups")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), legend.title = element_text(size = 14, face = "bold"))+
  scale_color_discrete(name = "Treatment Group", 
                       labels = c("Placebo", "Medication A", "Medication B"))+
  scale_linetype_discrete(name = "Model", 
                          labels = c("No Interaction", "Interaction"))

dev.off()

#### Two-Way ANOVA with sex as covariate ####
# Visualizing the differences in BP for each medication for both men and women
ggplot(bp, aes(x = Treatment, y = BP, fill = Sex)) +
  geom_violin(trim = FALSE)

# Adding the data points
png(file = "sex_violin_plot.png",
    width = 800, height = 800)

bp %>%
ggplot(aes(x = Treatment, y = BP, fill = Sex)) +
  geom_violin(trim = FALSE) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', 
               position = position_dodge(1), binwidth = 0.5) +
  labs(x = "Treatment Group", y = "Blood Pressure")+
  scale_x_discrete(label = c("Placebo", "Medication A", "Medication B"))+
  scale_fill_discrete(name = "Sex", labels = c("Female","Male"))+
  ggtitle("Effect of Sex on Treatment Effectiveness")+
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"), legend.title = element_text(size = 14, face = "bold"))
dev.off()

# Interaction Plot for each treatment group across Sex with 95% confidence intervals

png(file = "sex_effect_plot.png",
    width = 800, height = 800)

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