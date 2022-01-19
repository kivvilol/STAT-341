## CHAPTER 6 Logistic Regression

# Use this when Y is categorical (2 levels)
#   - ex) Y = male/female


# Goal: create a model that estimates the probability of an outcome occurring

library(regclass)

data(POISON)
head(POISON)

# generalized linear regression (logistic curve)
result <- glm(Outcome ~ Dose, data = POISON, family = 'binomial')
visualize_model(result)
summary(result)


library(ISLR)

data(Default)
head(Default)

result <- glm(default ~ balance, data = Default, family = 'binomial')
visualize_model(result)
summary(result)

confusion_matrix(result)

table(Default$default)

result2 <- glm(default ~ 1, data = Default, family = 'binomial')
confusion_matrix(result2)

check_regression(result)
# model fits well, p-value is 0.346

result3 <- glm(default ~ balance + income + student, data = Default, family = 'binomial')
summary(result3)
# dummy variable for student <- x3 = 1 if yes, student
# model shows students are less likely to default 
# OR = e^-0.647 = 0.5236
# 1/0.5236 - 1.91

# non students are 1.91 times more likely to default compared to students (if the people have the same balance and income)

boxplot(balance ~ student, data = Default, col = c('yellow', 'green'))


result4 <- glm(default ~ balance + student, data = Default, family = 'binomial')
summary(result4)

visualize_model(result4)
confusion_matrix(result4)
# false = 39/1000 = 0.039
# false = 228 / 10000 = 0.0228

check_regression(result4)
# p-value = 0.238, this is good 



































