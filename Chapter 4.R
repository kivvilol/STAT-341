## SCATTERPLOT AND CORRELATION MATRICES

# When we have multiple predictors (X1, X2, ...) for Y, we need to look at the relationships between
# Y and each X and also between the X's

# cor(), cor_matrix(), all_correlations(), pairs()

library(regclass)

data(SALARY)
head(SALARY)

cor(SALARY[,1:3])

cor_matrix(SALARY)

all_correlations(SALARY)

pairs(SALARY)

result <- lm(Salary ~ Education + Experience, data = SALARY)
summary(result)

# fitted multiple regression
#  yhat = 3097.93 + 89.98X1 + 13.17X2
#     - average value of Y among all individuals with a common set of characteristics is a 
#       weighted sum of our predictor variables (X's) - regression coefficients = weights

## Model Assumptions
#   - Linearity
#   - equal spread
#   - normality
#   - independence

## Residual Plots check_regression()
# If you see curvature, try transforming X. If you see unusual spread, try transforming Y.


check_regression(result, extra = TRUE)

## IMPORTANT
#   - r2 will always increase in multiple regression. Pay attention to the SIZE of the increase.

#   - r2adj is r2 minus a penalty, which is based on number of predictors added to the model. 

## INFERENCE

# For multiple regression, the standard error for coefficient Bi depends on how well the model for the data AND the amount
# of correlation and redundant information in the predictors

# If Xi can be predicted accurately from the other X's, then Ri^2 will be large and the information Xi provides
# about y will already be covered by the other X's


## PARTIAL F-TEST FOR INDIVIDUAL PREDICTORS OR GROUPS

# SSEsimple = SSE for model with fewer predictors
#   s = number

# SSEcomplex = SSE for model with the added predictor(s)
#   c = # of predictors in the complex model

# therefore
#   c > s

# F-test for overal modell significance

# F = ((SSEnaive - SSEreg) / k) / (SSEreg / n-k-1)
#     - k = # of predictors in model

# Partial F-test statistic

# F = ((SSEsimple - SSEcomplex) / (c-s)) / (SSEcomplex / (n-c-1))

#     - F is large <- something in model predict Y 
#         - amount of explained error > amount left unexplained

# Partial F-test for individual and groups of predictors

# F = (SSEsimple - SSEcomplex) / (c-s)) / (SSEcomplex / (n-c-1))
#     - F is large <- added predictor helps predict Y


# In test, we look at what values of F we could get if adding the predictor does not really help
#     - Compare our f to that
#       - If our F is large enough to mean predictor is significant?
#           - p value < 0.05 <- yes, predictor does belong in model


library(regclass)
data(SALARY)
data(NFL)

head(SALARY)

# check overall model significance
result <- lm(Salary ~ Education + Experience, data = SALARY)
summary(result)

# check partial model significance
# Is education significant when experience is already in the model?

result_simple <- result <- lm(Salary ~ Experience, data = SALARY)
result_complex <- result <- lm(Salary ~ Education + Experience, data = SALARY)
anova(result_simple, result_complex)
# education is significant


# check partial model significance
# Is education significant when experience is already in the model?

result_simple <- result <- lm(Salary ~ Education, data = SALARY)
result_complex <- result <- lm(Salary ~ Education + Experience, data = SALARY)
anova(result_simple, result_complex)
# experience is significant

# Larger value of f statistic means a larger reduction in SSE of the model


# NFL
help(NFL)
# 1) X70.DefTot1stDwnsAlwd
# 2) X10.OffFumblesLost
# 3) X13.OffPassYds
# 4) X41.OffAllPurposeYds
# 5) X23.OffPassSacksAlwd
# 6) X111.DefYdsPt

# overall f statistic test
result_nfl <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X10.OffFumblesLost + X13.OffPassYds + X41.OffAllPurposeYds + 
                X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
summary(result_nfl)
# our overall model is significant! F = 140.7, p-value = 0


# Partial f-test for Defense related predictors
result_simple <- lm(X4.Wins ~  X10.OffFumblesLost + X13.OffPassYds + X41.OffAllPurposeYds + 
                                    X23.OffPassSacksAlwd, data = NFL)
result_complex <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X10.OffFumblesLost + X13.OffPassYds + X41.OffAllPurposeYds + 
                                     X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
anova(result_simple, result_complex)
# at least one of our defense related predictors is significant 


# Partial f-test for Offense related predictors
result_simple <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X111.DefYdsPt, data = NFL)
result_complex <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X10.OffFumblesLost + X13.OffPassYds + X41.OffAllPurposeYds + 
                       X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
anova(result_simple, result_complex)
# at least one of our offense related predictors is significant


# t-tests for individual predictors (equivalent to partial F-test)
result_nfl <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X10.OffFumblesLost + X13.OffPassYds + X41.OffAllPurposeYds + 
                   X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
summary(result_nfl)

# X10.OffFumblesLost not significant given all other observations
# X13.OffPassYds not significant given all other observations

# Remove ONE predictor! Remove X10.OffFumblesLost (largest p-value)
result_nfl <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X13.OffPassYds + X41.OffAllPurposeYds + 
                   X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
summary(result_nfl)
# X13.OffPassYds still not significant given all other observations

# Remove X13.OffPassYds
result_nfl <- lm(X4.Wins ~  X70.DefTot1stDwnsAlwd + X41.OffAllPurposeYds + 
                   X23.OffPassSacksAlwd + X111.DefYdsPt, data = NFL)
summary(result_nfl)


# RMSE = 1.671 <- predictions will be off, on average, by about 1.671 games
# R^2 = 0.7086 <- These 4 predictors (together) explain 70.86% of the variation in the number of wins


check_regression(result_nfl, extra = TRUE)


## REVIEW

# Y = Number of Wins
# X1 = X70.DefTot1stDwnsAlwd
# X2 = X41.OffAllPurposeYds
# X3 = X23.OffPassSacksAlwd
# X4 = X111.DefYdsPt

# Fitted Model
#   - Y = 12.772 - .0267x1 + 0.002x2 - 0.0456x3 + 0.938x4

# 95% CI for beta4 (coefficient for x4)
# 0.938 +/- 2(0.052) = (0.834, 1.042)
#   - Interpret: Two that have the same DefTot1stDwnsAlwd, OffAllPurposeYds, and OffPassSacksAlwd, but differ in DefYdsPt
#                by 1, we'd expect their average number of wins to differ by between 0.834 and 1.042


pairs(NFL[, c(1, 20, 38, 67, 108)])
cor_matrix(NFL[, c(1, 20, 38, 67, 108)])
all_correlations(NFL[, c(1, 20, 38, 67, 108)])


library(regclass)

data(AUTO)
head(AUTO)

# predict top speed using weight
result <- lm(TopSpeed ~ Weight, data = AUTO)
summary(result)
# Y = 76.25 + 1.17X


# predict top speed using weight and horsepower
result2 <- lm(TopSpeed ~ Weight + Horsepower, data = AUTO)
summary(result2)
VIF(result2)
# Y = 96.42 - 0.71X1 + 0.32X2
# VIF1 = VIF2 = 3.25 = sqrt(3.25) = 1.8
#   - standard error is 1.8 times larger than if our variables (weight and horsepower) were completely unrelated to each other


## VIF - Variance Inflation Factor
#   - quantifies the increase in SE due to correlation among X's


# predict top speed using weight, horsepower, and fuel efficiency
result3 <- lm(TopSpeed ~ Weight + Horsepower + FuelEfficiency, data = AUTO)
summary(result3)
VIF(result3)
# Y = 110.47 - 0.91x1 + 0.32x2 - 0.21x3


## Increasing the flexibility of the model

data(BULLDOZER)
head(BULLDOZER)

# develop Kelly blue book for bulldozers sold at auctions
#   - Predict sale price based on:
#       - Years ago auction occurred
#       - Year made - model year
#       - Usage - level of use (in hours)
#       - Blade - blade width (ft)
#       - Tire - tire size (in)

# look at pairs
pairs(BULLDOZER)

# build multiple regression model using all predictors
bulldozer_result <- lm(SalePrice ~ YearsAgo + YearMade + Usage + Blade + Tire, data = BULLDOZER)
summary(bulldozer_result)
check_regression(bulldozer_result, extra = TRUE)
# RMSE = 22120, R^2 = 0.58, bs = 2510, SE =  37.15
#     - If our r^2 is 0.58, 58% of our variation is explained by our 5 predictors.
#     - For RMSE, our estimate will be off, on average, by $22,120
#     - Our tire coefficient tells us that the sale price between 2 tires that differ by 1 inch differs by $2,510
#     - We are 95% confident that sale price is $2,510 +/- 37.15
# Our overall model is significant! F-statistic = 254.2 and p-value = 0


# checking if blade and usage matter together (Partial F-test)
bulldozer_result_simple <- lm(SalePrice ~ YearsAgo + YearMade + Tire, data = BULLDOZER)
bulldozer_result_complex <- lm(SalePrice ~ YearsAgo + YearMade + Usage + Blade + Tire, data = BULLDOZER)
anova(bulldozer_result_simple, bulldozer_result_complex)
# F-statistic = 0.0264, p-value = 0.974
# Taken together, they do not contribute. Therefore, it is fine to remove both

summary(bulldozer_result_simple)


## INCREASING THE FLEXIBILITY OF THE MODEL

#   - Simple linear regression <-  transformations to 'fix' non-linearities and equal spread
#   - Multiple regression - can include polynomial terms (x, x^2, x^3) and combinations of x's (x1, x2, x3, x1/x2, x2/x3, etc.)

# BULLDOZER EXAMPLE
data(BULLDOZER)
head(BULLDOZER)

# Y = sales price
# X1 = years ago
# X2 = year made
# x3= tire

result <- lm(SalePrice ~ YearsAgo + YearMade + Tire, data = BULLDOZER)
summary(result)
check_regression(result, extra = TRUE)

result_poly <- lm(SalePrice ~ YearsAgo + YearMade + I(YearMade^2) + Tire, data = BULLDOZER)
summary(result_poly)
check_regression(result_poly, extra = TRUE)



data(SOLD26)
head(SOLD26)

# predict SoldWeek26
# StoresSelling26

pairs(SOLD26[1:10])

hist(log(SOLD26$SoldWeek26))

result <- lm(log(SoldWeek26) ~ . , data = SOLD26)
summary(result)
check_regression(result, extra = TRUE)


library(boot)

data(melanoma)
head(melanoma)


library(regclass)

data(TIPS)
head(TIPS)

# predicting tippercentage

result <- lm(TipPercentage ~ Bill + Bill*PartySize, data = TIPS)

visualize_model(result)

summary(result)


library(carData)

data(Highway1)
head(Highway1)

# predict rate using len, slim, acpt
result <- lm(rate ~ len*slim*acpt, data = Highway1)
summary(result)
# overall model is significant
# 3-way interaction is not significant

result_simple <- lm(rate ~ len + slim + acpt, data = Highway1)
result_complex <- lm(rate ~ len + slim + acpt + len*slim + len*acpt + slim*acpt, data = Highway1)
anova(result_simple, result_complex)
# group of 2-way interaction terms is not significant, so remove all of them


result_simple <- lm(rate ~ len + slim + acpt, data = Highway1)
summary(result_simple)
# all predictors are significant in the simple model


# How well does our model fit the data?
# r^2 = 0.7008 <- 70% of our variation in rate is explained by our model using len, slim, and acpt as predictors
# RMSE = 1.132 <- Is this a large error if we're predicting rate?

# Are the regression assumptions met?
check_regression(result_simple, extra = TRUE)
# We see some problems with equal spread, so the standard deviation of the residuals is not constant across
# all predicted y-values 
# Problem might be with acpt.

hist(Highway1$rate)
# could also try transforming y to make distribution more symmetric. 

data(BODYFAT)
head(BODYFAT)

cor_matrix(BODYFAT)
# Abdomen, Weight, Chest, Hip, Thigh, Knee

result <- lm(BodyFat ~ Abdomen + Weight + Chest + Hip + Thigh + Knee, data = BODYFAT)
summary(result)
# only 3 variables appear to be significant (abdomen, weight, chest)
# test if the group is significant before we remove them

# 3 separate tests - 3 chances to be wrong - 0.05^3
# 1 test - chance of making an error is much lower - 0.05

# Also, significance of predictors depends on what else is in the model. 
















