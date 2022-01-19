## CATEGORICAL VARIABLES AS PREDICTORS

library(regclass)


data(SALARY) # Gender is an important factor
head(SALARY)

# To get gender into the model, create a dummy variable (or indicator variable)
#   - Basically, convert categorical into 0 and 1 (r does this alphabetically)

# Predict salary using education and gender

boxplot(Salary ~ Gender, data = SALARY)

str(SALARY) # data frame, gender is factor data

result <- lm(Salary ~ Education + Gender, data = SALARY)
summary(result)

visualize_model(result)

# Y = 3212.47 + 53.80x1 + 461.21x2

#   - 461.21x2 = Difference intercepts for regression lines of male and female

#   - Female regression line <- y = 3212.47 + 53.80x1
#   - Male regression line   <- y = 3212.47 + 53.80x1 + 461.21
#                               y = 3673.68 + 53.80x1



# checking interaction terms
result <- lm(Salary ~ Education*Gender, data = SALARY)
summary(result)

visualize_model(result) # The slope of female is smaller than male, but the difference is not large enough to be significant.

# Y = 3261.58 + 41.42x1 + 209.22x2 + 49.06x3

#   - Female regression line <- y = 3261.58 + 41.42X1
#   - Male regression line   <- y = 3261.58 + 41.42x1 + 209.22 + 49.06x1
#                               y = 3470.80 + 90.48


# Add in experience + the interaction between education and experience
#   - overall model significant?
#   - interaction term significant? (if yes, must keep education + experience in the model too)
#   - gender significant?

result <- lm(Salary ~ Education + Gender + Experience + Education*Experience, data = SALARY)
summary(result)

# interaction term is not significant

result <- lm(Salary ~ Education + Experience + Gender, data = SALARY)
summary(result)
# all terms are significant after removing the interaction term



data(TIPS)
head(TIPS)
str(TIPS)

# day of the week may be important when trying to predict tip percentage
#   - Weekday is a categorical variable with 4 levels: Thursday, Friday, Saturday, Sunday
#   ** Since there are more than 2 levels, we create multiple dummy variables
#   - levels - 1 = number of dummy variables needed. 

result <- lm(TipPercentage ~ Bill + Weekday, data = TIPS)
summary(result)
visualize_model(result, loc = 'topright')

# partial f test of Weekday
drop1(result, test = 'F')

# fitted model
#   Y = 21.106 - 0.239x1 - 0.897x2 + 0.711x3 - 0.747x4

#   Interpret <- 21.106 = intercept when it is Friday
#             <- -0.239 = relationship between bill and tippercentage
#             <- -0.897 = difference in average tippercentage between Saturday and Friday  
#             <-  0.711 = difference in average tippercentage between Sunday and Friday
#             <- -0.747 = difference in average tippercentage between Thursday and Friday

#   We ran a partial F-test to determine whether the group of dummy variables was significant.
#   Used drop1(). This is called 'effect test' 
#       - p-value = 0.30
#           - Weekday not significant, so we can take it out of the model.


library(regclass)
data(ATTRACTM)
head(ATTRACTM)

# Study relationship between attractiveness score (Score)
# and fashionableness (FashionScore)
# Its strength may vary depending on whether the person
# has a visible tattoo (tattoo)


# defining dummy variable
# X2 = 0 if no
# X2 = 1 if yes

str(ATTRACTM)

# find the fitted model
result <- lm(Score ~ FashionScore + Tattoo, data = ATTRACTM)
summary(result)
visualize_model(result)
## Fitted model <- Y = 2.309 + 0.072x1 + 0.019x2

# Implicit x2 = 0 <-  2.309 + 0.072x1
# Implicit x2 = 1 <-  2.328 + 0.072x1
#   - Among men that have the same fashion score, those with a visible tattoo are 
#     gonna score higher, on average, by 0.019

## the intercepts are different, however not by very much.


## Does relationship between attractiveness score and fashionableness 
## depend on whether the person has a visible tattoo?

result2 <- lm(Score ~ FashionScore*Tattoo, data = ATTRACTM)
summary(result2)
visualize_model(result2, loc = 'bottomleft')
## Fitted model <- Y = 2.283 + 0.083x1 + 0.539x2 - 0.504x3

# Implicit x2 = 0 <-  Y = 2.2839 + 0.08314x1
# Implicit x2 = 1 <-  Y = (2.2839 + 0.53851) + (0.08314 - 0.50419)
#                       - 2.8224 - 0.42105x1

# Interpret b2 = 0.539
#   - Among men that have the same fashion score, those with a visible tattoo are gonna score
#     higher, on average, by 0.539.

# Interpret b3 = 0.504
#   - Difference in slopes for two implicit regression lines

## The slopes are very different, so there appears to be an interaction

# test whether the interaction term is significant


data(STUDENT)
head(STUDENT)

# Dummy Variables??? 3 
# Reference Level?

result <- lm(CollegeGPA ~ HSGPA + Pet, data = STUDENT)
visualize_model(result)
summary(result)

drop1(result, test = 'F')

# changing reference level
STUDENT_alt <-  STUDENT
levels(STUDENT_alt$Pet)[levels(STUDENT_alt$Pet) == 'Both'] <- 'Either'
STUDENT_alt$Pet <- factor(STUDENT_alt$Pet, levels(STUDENT_alt$Pet))[c(2, 3, 1, 4)]
levels(STUDENT_alt$Pet)

result <- lm(CollegeGPA ~ HSGPA + Pet + Gender, data = STUDENT)
summary(result)





























