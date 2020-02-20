
#BANK LOAN DEFAULT


rmlist=ls()

library(ggplot2)
library(corrgram)
library(gridExtra)
rmlist=ls()

library(ggplot2)
library(corrgram)
library(gridExtra)

setwd("D:/Data Science/Python")

getwd()

loan = read.csv("bank-loan.csv")

View(loan)

#Only Numeric data is their so no data changes required.


sum(is.na(loan$age))

sum(is.na(loan$ed))

sum(is.na(loan$employ))

sum(is.na(loan$address))

sum(is.na(loan$income))

sum(is.na(loan$debtinc))

sum(is.na(loan$creddebt))

sum(is.na(loan$othdebt))

sum(is.na(loan$default))

#Imputing the values

loan$default[is.na(loan$default)] <- mean(loan$default, na.rm = T)

#Checking, if its been updated or not

sum(is.na(loan$default))

#The same can also be checked with the help using funtion : complete.case

sum(complete.cases(loan))

#Upon validating, we clearly see that there are no NA records in dataset

#Outlier analysis

OutVals = boxplot(loan)

#Observations: There are few outliers therefore We will go with Outliers

dim(loan)

summary(loan)

#Correlation

corr_df  =  cor(loan)    #cor compute the variance and the covariance or correlation 
View(corr_df)

#Observation: There is some correlation between Outcome variable and other variables.

#1. Mean and Median are nearly equal after doing data imputation whicih help to reduce Outliers.
#2. Data summary is looking good we can continue with logistic model

stploan=step(glm(data=loan, formula=default~., family=binomial), trace=0, steps=100)

summary(stploan)

#Observation:Best results given by default ~ employ + address + debtinc + creddebt

#Make Final Multi Linear Model

mgmloan = glm(data=loan, formula=default ~ employ + address + debtinc + creddebt, family=binomial(link="logit"))

summary(mgmloan)

pR2 = 1 - mgmloan$deviance / mgmloan$null.deviance

pR2

#the confint function to obtain confidence intervals for the coefficient estimates

confint(mgmloan)

#logistic regression

predict = predict(mgmloan, type = 'response')

table(loan$default, predict > 0.5)


fitted.results <- predict(mgmloan,type='response')
head(fitted.results)


fitted.results <- ifelse(fitted.results > 0.5,1,0)

head(fitted.results)

misClasificError <- mean(fitted.results != loan$default)

misClasificError

print(paste('Accuracy',1-misClasificError))
