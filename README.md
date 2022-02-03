# Cox-and-AFT-models
library(haven)
library ( survival )
library ( survminer )
library(tidyverse)
library(knitr)
library(ciTools)
library(here)

#Import data
data<- read_sav("C:/Users/OBITE C. P/Desktop/Research/marriage to first birth interval/marriage to first birth main.sav")

#The Cox model
cox.rep<- coxph(Surv(Marriage_Birth, Status) ~  Age_Marriage 
                + factor(Religion) + factor(Region) + factor(Residence) + factor(Education) + First_Sex
                + factor(Husband_Education) + factor(Contraceptive) , data = data, x = TRUE, y = TRUE)

#The proportional hazard assumption
cox.prop<- cox.zph(cox.rep, transform = "km", global = TRUE)
cox.prop

#The AFT model
aft.rep<- survreg(Surv(Marriage_Birth, Status) ~  Age_Marriage 
                  + factor(Religion) + factor(Region) + factor(Residence) 
                  + factor(Education) + First_Sex
                  + factor(Husband_Education) + factor(Contraceptive) 
                , data = data, dist = "weibull")
summary(aft.rep)

#Kaplan Plot
plot(survfit(Surv(data$Marriage_Birth, data$Status)~1), xlab = "Time", ylab = "Survival probability")
hist(data$Marriage_Birth[which(data$Status == 1)], xlab = "Time", main = "")



