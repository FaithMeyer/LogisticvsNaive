install.packages("gdata")
library(gdata)
library(dplyr)
library(naivebayes)
library(ggplot2)
library(ISLR)
# Working Directory/Load
setwd("C:/Users/faith/Documents/Advanced Data Science")
Hitters
head(Hitters)
distinct(Hitters)
#Naive Bayes
indexes <- sample(2,nrow(Hitters), replace = T, prob = c(0.8, 0.2))
trainball <- Hitters[indexes==1, ]
testball <- Hitters[indexes==2, ]

model <- naive_bayes(Division ~Years, data = trainball, usekernel = T)
plot(model)

p <- predict(model, testball)
table(p, testball$Division)

#Logistic
glm.fit <- glm(Division~Years, data = trainball, family = binomial)

summary(glm.fit)

glm.probs <- predict(glm.fit, testball, type = "response")
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, "E", "W")
attach(testball)
table(glm.pred,testball$Division)

#Change E and W to 0 and 1
as.numeric(Hitters$Division)
gdata::mapLevels(Hitters$Division)
as.numeric(Hitters$Division) - 1

#Plots
#Logistic Regression
ggplot(Hitters, aes(x=Years, y=as.numeric(Division) - 1)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  ylab("Division") 
#Key: 0=E, 1=W

#Naive Bayes
ggplot(Hitters, aes(Years, fill=(Division)))+
  geom_density(alpha=.5)
  


