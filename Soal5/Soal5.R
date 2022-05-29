library(readr)
library(multcompView)
library(dplyr)
library(ggplot2)

gtl <- read.csv(file.choose())

#5A
qplot(x = Temp, y = Light, geom = "point", data = gtl) + 
  facet_grid(.~Glass, labeller = label_both)

#5B
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gt)

anova <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(anova)

#5C
data_sumarry <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_sumarry)

#5D
print("tukey test : ")
tukey <- TukeyHSD(anova)
print(tukey)

#5E
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)