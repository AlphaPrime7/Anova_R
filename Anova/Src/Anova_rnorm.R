#Theme: Anova
#set the working directory
setwd("~/rlearn")

#Generate the data set using rnorn
r1 <-rnorm(89, runif(1, 66.6, 70.8), 10.98) #rnorm(n,mean,sd)
r2 <-rnorm(67, runif(1, 70.46, 75.8), 8.9)
r3 <-rnorm(125, runif(1, 90.6, 101.4), 7.9)
r4 <-rnorm(98, runif(1, 135.4, 154.5), 8.98)
data.Q1 <-cbind.data.frame(c(rep("A", length(r1)),rep("B", length(r2)),rep("C", length(r3)),rep("D", length(r4))), round(c(r1, r2, r3, r4), 2))
colnames(data.Q1)<-c("Population","Mass_kg")

#Data exploration using dplyr
install.packages('dplyr')
library(dplyr)
data.Q1 %>% group_by(Population) %>% summarise(mean = mean(Mass_kg), sd = sd(Mass_kg))

#Data exploration fun using ggplot
install.packages('ggplot2')
library(ggplot2)
require(ggplot2)
plot <- ggplot(data=data.Q1, aes(x=Population, y= Mass_kg)) + geom_boxplot(aes(fill=Population))
print(plot + labs(title = "Mass_Kg vs Supplement Formulation", x = "Supplment Formulation", y = "Mass_kg", fill ="Formulation"))
stripplot <- ggplot(data.Q1, aes(x=Population, y=Mass_kg)) + 
  geom_jitter(position=position_jitter(0.2), shape=18, cex=3)
stripplot <- ggplot(data.Q1, aes(x=Population, y=Mass_kg, shape=Population)) + geom_jitter(position=position_jitter(0.2), cex=3)
stripplot + scale_shape_manual(values=c(1,17,19,20))
stripplot + stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")
print(stripplot + labs(title = "Mass_Kg vs Supplement Formulation", x = "Supplment Formulation", y = "Mass_kg", shape ="Formulation") + stat_summary(fun.y=mean, geom="point", size=3, color="blue"))


#fit the anova model
model <- aov(Mass_kg ~ Population, data = data.Q1)
summary(model)

#Test for normality
plot(model)

#Lavene's test using car package
install.packages('car')
library(car)
leveneTest(Mass_kg ~ Population, data = data.Q1)

#Tukey's test for multiple comparisons
TukeyHSD(model, conf.level=.95) 
plot(TukeyHSD(model, conf.level=.95), las = 2)