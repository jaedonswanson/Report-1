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

