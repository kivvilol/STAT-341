library(regclass)
library(ISLR)
library(AER)

data(Carseats)
head(Carseats)
str(Carseats)

result <- lm(formula = Sales ~ Price + ShelveLoc + Advertising, data = Carseats[, -11])
summary(result)

data(Affairs)
head(Affairs)

ynAffairs <- Affairs
ynAffairs$affair[ynAffairs$affairs > 0] <- 1
ynAffairs$affair[ynAffairs$affairs == 0] <- 0
ynAffairs$affair <- factor(ynAffairs$affair, levels = c(0, 1), labels = c('Yes', 'No'))
head(ynAffairs)

result2 <- glm(formula = affair ~ gender + age + yearsmarried + religiousness + rating, data = ynAffairs, family = "binomial")
result3 <- glm(formula = affair ~ rating, data = ynAffairs, family = "binomial")
summary(result2)
head(ynAffairs)

str(ynAffairs)

exp(coef(result3))

newdata = data.frame(gender = 'male', age = 28, yearsmarried = 3, religiousness = 4.5, rating = 2)

predict(result2, newdata, type = 'response')
