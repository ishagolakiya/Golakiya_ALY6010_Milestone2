print("Isha Golakiya")
print(" Diabetes Analysis")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(Rmisc)
library(psych)
library(ggpubr)

#Reading dataset
setwd("/Users/HP/Downloads")
diabete <- read.csv("diabetes.csv")
View(diabete)
headtail(diabete)

#Information regarding dataset
dim(diabete)
str(diabete)

#cleaning data
# checking missing values
cat("Number of missing value:", sum(is.na(diabete)), "\n")
vis_miss(diabete)

class(diabete$BloodPressure)

#converting to numeric
diabete$BloodPressure <- as.numeric(diabete$BloodPressure)
diabete$BloodPressure
class(diabete$BloodPressure)
class(diabete$BMI)
diabete$BMI <- as.numeric(diabete$BMI)
diabete$BMI
class(diabete$Age)
diabete$Age <- as.numeric(diabete$Age)
class(diabete$Age)
diabete$Age
diabete$Pregnancies <- as.numeric(diabete$Pregnancies)
class(diabete$Pregnancies)
diabete$Glucose <- as.numeric(diabete$Glucose)

class(diabete$Glucose)


#removing unrealistic values
diab <- diabete[(diabete$BloodPressure > 0) & (diabete$BMI > 0),]
diab
summary(diab)
str(diab)

headtail(diab,5)

diabetes <-filter(diab,diab$Outcome == '1')
summary(diabetes)
str(diabetes)

plot(diabete$BloodPressure,ylab = "Blood Pressure")

#one sample

mean(diabetes$BloodPressure)
sd(diabetes$BloodPressure)

onesam <- sample_n(diabetes, 20)

t.test(onesam$BloodPressure, mu = 75.239, alternative = 'two.sided')
qt(p=0.05/2, df=20, lower.tail = FALSE)



# two-sample t-test

elder <- subset(diabetes, subset = (diabetes$Age >=30))
elder
elderm <- sample_n(elder,20)

young <- subset(diabetes, subset = (diabetes$Age < 30))
young
youngch <- sample_n(young, 20)
shapiro.test(elderm$Glucose)
shapiro.test(youngch$Glucose)
ggqqplot(elderm$Glucose, ylab = "Elder's Glucose",
         ggtheme = theme_minimal())
t.test(youngch$Glucose,elderm$Glucose, alternative = "two.sided")
qt(p=0.05/2, df=20, lower.tail = FALSE)
