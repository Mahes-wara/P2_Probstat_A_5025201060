myFile <- read.delim(file.choose())

myFile$Group <- as.factor(myFile$Group)
myFile$Group = factor(myFile$Group,labels = c("grup1", "grup2", "grup3"))

class(myFile$Group)

#4A
grup1 <- subset(myFile, Group == "grup1")
grup2 <- subset(myFile, Group == "grup2")
grup3 <- subset(myFile, Group == "grup3")

qqnorm(grup1$Length)
qqline(grup1$Length)

qqnorm(grup2$Length)
qqline(grup2$Length)

qqnorm(grup3$Length)
qqline(grup3$Length)

#4B
bartlett.test(Length~Group, data = myFile)

#4C
model1 = lm(Length~Group, data = myFile)
anova(model1)

#4F
library("ggplot2")

ggplot(myFile, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "green", colour = "yellow") +
  scale_x_discrete() + xlab("Group") + ylab("Length")
