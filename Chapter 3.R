## SIMPLE LINEAR REGRESSION

# Regression is a model association between X and Y

# Simple linear regression model relates the average value of Y to the predictor, 
# or exploratory, variable X
#   - y = b0 + b1(x)
#       - y = intercept + slope(x)

## PHILOSOPHY OF MODELING

# Descriptive vs. Predictive modeling


## Interpretation of Beta coefficients 

# b0 = average value of y when x = 0 (y-intercept)
# b1 = expected or average difference in y between two individuals who differ by one unit of x

# STEPS TO COMPLETE
#   1) test for linear association (SCATTERPLOT)
#   2) test for a statistically significant correlation (associate)
#   3) Find estimates for b0 and b1

library(regclass)

data(SALARY)
head(SALARY)

plot(SALARY$Education, SALARY$Salary)
abline(results, col = 'red', lwd = 2) # adding regression line from results

cor(SALARY$Education, SALARY$Salary)

associate(Salary ~ Education, data = SALARY)

results <- lm(Salary ~ Education, data = SALARY)
summary(results) # intercept (b0) = 3228.83, education (b1) = 85.39
#   - b0 = 3228.83 : average salary of a person with no college experience (graduated HS) is $3228.83
#   - b1 = 85.39 : two employees who differ by 1 year of education are expected to differ in salary by $85.40
#       - This is because slope estimate is positive, more education <- higher salary


plot(SALARY$Months, SALARY$Salary)
abline(results2, col = 'red', lwd = 2)
cor(SALARY$Months, SALARY$Salary)

associate(Salary ~ Months, data = SALARY)
results2 <- lm(Salary ~ Months, data = SALARY)
summary(results) # b0 = 3393.07, b1 = 13.186
#   - b0 = 3393.07 : average salary of a person with no months of experience is $3393.07
#   - b1 = 13.186 : two employees who differ in experience by 1 month are expected to differ in salary by $13.18


# How does r calculate b0 and b1
#   - It find the least squares line (line closest to the data as a whole)
#       - This means it finds the line that minimizes the sum of the squared error(residuals), also knows as (SSE)
#           - residuals = actual value of Y minus predicted value of Y


# SSE = sum([y(i) - (b0 + b1*x(i))]^2)

# How to minimize with respect to b0 and b1?
#   - Take partial derivative of SSE with respect to b0 and b1
#   - Set = 0
#   - Solve system of equation
#  1) dSSE/db0 = -2 sum(y(i) - b0 - b1x(i)) = 0
#  2) dSSE/db1 = -2 sum(y(i)x(i) - b0(x(i)) - b1x(i)^2) = 0
#     b1 = r(SDy/SDx)
#     b0 = y(avg) - b1*x(avg)

# More precise w/ notation for regression model
#   - mean(yix) + Ei (residuals)

data(TIPS)
head(TIPS)
help(TIPS)

# Can we predict the average tip when the bill is $25?

# First - scatterplot! Are tip and bill linearly related
plot(TIPS$Bill, TIPS$Tip)
result <- lm(Tip~Bill, data = TIPS)
summary(result)
mean(TIPS$Bill)
var(TIPS$Bill)
sd(TIPS$Bill)

visualize_model(result)

predict()

check_regression(result)
find_transformations(result)


