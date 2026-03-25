# initial import of data to R
bp <- read_csv("data.csv")

# changing the treatment column so R sees it as a categorical variable
bp$Treatment <- as.factor(bp$Treatment)

# Visualizing the differences in BP for each medication for both men and women
ggplot(bp, aes(x = Treatment, y = BP, fill = Sex)) +
  geom_violin(trim = FALSE)

# Adding the data points
ggplot(bp, aes(x = Treatment, y = BP, fill = Sex)) +
  geom_violin(trim = FALSE) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', 
               position = position_dodge(1))
#### ANCOVA with Age as Covariate ####


#### Two-Way ANOVA with sex as covariate ####
# Boxplots to visualize the difference
boxplot(BP ~ Sex:Medication,
        data = bp,
        main = "Blood Pressure Distribution by Group",
        xlab = "Group",
        ylab = "Blood Pressure",
        col = "firebrick")