library(boot)
library(regclass)

# importing data
data(melanoma)
head(melanoma)

# creating uncensored data (patients that did die from melanoma)
melanoma_uncensored <- melanoma[melanoma$status == 1, c(1, 3:7)]
head(melanoma_uncensored)

# converting numeric to character data
class(melanoma_uncensored$sex) <- 'character'
class(melanoma_uncensored$ulcer) <- 'character'
str(melanoma_uncensored)


# histogram of survival time
hist(melanoma_uncensored$time) # we can see that our data is slightly right skewed, maybe log transform

# summary statistics and standard deviation of survival time
summary(melanoma_uncensored$time) # 1062 Median, 1253 Mean, 185 Min, 3338 Max
sd(melanoma_uncensored$time) # 759 standard deviation

# histogram of average age
hist(melanoma_uncensored$age) # normally distributed, as expected 
mean(melanoma_uncensored$age) # average age is 55 


# histogram of operation year
hist(melanoma_uncensored$year) # nothing clear here
mean(melanoma_uncensored$year) # average year of operation is 1969


# qqplot to check normality
qqnorm(melanoma_uncensored$time)
qqline(melanoma_uncensored$time) ## seems a bit off towards the end, but we will see more later

# association testing 
associate(time ~ thickness, data = melanoma_uncensored) # histogram looks good, p-value = 0.01
                                                        # no clear pattern, maybe a slight negative
                                                        # Our r is -0.38
plot(time ~ thickness, data = melanoma_uncensored)


associate(time ~ age, data = melanoma_uncensored) # plot looks randomly scattered, no clear pattern
                                                  # our r is -0.006
plot(time ~ age, data = melanoma_uncensored)


associate(time ~ ulcer, data = melanoma_uncensored) # clear difference between our box plots
                                                    # our qqplots have some slight issues towards end
                                                    # p-value = 0.028, 95% CI is (0.015, 0.047)
                                                    # our discrepancy is 4.987


associate(time ~ sex, data = melanoma_uncensored)   # clear difference between boxplots
                                                    # qqplots don't look terrible
                                                    # p-value 0.24, 95% CI is (0.203, 0.28)
                                                    # our discrepancy is 1.613


associate(time ~ year, data = melanoma_uncensored)  # graphs look not great
                                                    # p value = 0.420, 95% CI 0.376, 0.465
                                                    # no clear pattern
plot(time ~ year, data = melanoma_uncensored)


## PROBLEM 2

# checking all correlations of time
all_correlations(melanoma_uncensored)


# creating a fitted regression model of time ~ thickness
result <- lm(time ~ thickness, data = melanoma_uncensored)
summary(result)

visualize_model(result)

check_regression(result)

# fitted regression model with log transformation
log_result <- lm(time ~ log(thickness), data = melanoma_uncensored)
summary(log_result)

check_regression(log_result)

# fitted regression model with squared transformation
square_root_result <- lm(time ~ sqrt(thickness), data = melanoma_uncensored)
summary(square_root_result)

check_regression(square_root_result)

# creating a 95% confidence and prediction interval to predict survival time 

newdata <- data.frame(thickness = 4.31) # mean value and thickness

predict(square_root_result, newdata, interval = 'confidence', level = 0.95) # confidence

predict(square_root_result, newdata, interval = 'predict', level = 0.95) # prediction


