library(regclass)
library(tidyverse)
library(psych)

insurance <- read.csv('insurance.csv', stringsAsFactors = T)
head(insurance)
str(insurance)

summary(insurance)

hist(insurance$age)

table(insurance$sex)


insurance$logCharges <- log(insurance$charges)
hist(insurance$logCharges)

pairs.panels(insurance)


result <- lm(logCharges ~., data = insurance)
summary(result)

check_regression(result)

## I changed the variables here instead of making more models
result2 <- glm(as.factor(logCharges) ~  age + as.factor(sex) + bmi
               + children + as.factor(smoker) + as.factor(region), 
               data = insurance, family = 'binomial')

summary(result2)
check_regression(result2)








