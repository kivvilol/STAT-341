library(regclass)

data(AUTO)
head(AUTO)

# scatterplot Matrix
pairs(AUTO)

# fitting the model
full_result <- lm(FuelEfficiency ~ . , data = AUTO)
summary(full_result) # Y = 192.438 - 0.016 + 0.392 - 1.295 - 1.860
check_regression(full_result)

# creating a polynomial term model
squared_result <- lm(FuelEfficiency ~ CabVolume + I(Horsepower^2) + I(Horsepower^3) + I(TopSpeed^2)
                     + I(TopSpeed^3) + Weight, data = AUTO)
summary(squared_result)
check_regression(squared_result)

# individual term significance (I'm switching out one variable instead of a new model for each one)
squared_result_simple <- lm(FuelEfficiency ~ CabVolume + I(Horsepower^2) + I(TopSpeed^2)
                            + I(TopSpeed^3) + Weight, data = AUTO)

squared_result_complex <- lm(FuelEfficiency ~ CabVolume + I(Horsepower^2) + I(Horsepower^3) + I(TopSpeed^2)
                             + I(TopSpeed^3) + Weight, data = AUTO)

anova(squared_result_simple, squared_result_complex)


poly_md <- lm(FuelEfficiency ~ CabVolume + I(Horsepower^2) + I(Horsepower^3) + I(TopSpeed^2)
              + I(TopSpeed^3) + Weight, data = AUTO)

summary(poly_md)

check_regression(poly_md, extra = TRUE)



## Question 4

data(EX4.BIKE)
head(EX4.BIKE)

bike_result <- lm(Demand ~ ., data = EX4.BIKE)
summary(bike_result)

averages <- data.frame(Demand = mean(EX4.BIKE$Demand), 
                       AvgTemp = mean(EX4.BIKE$AvgTemp), 
                       EffectiveAvgTemp = mean(EX4.BIKE$EffectiveAvgTemp), 
                       AvgHumidity = mean(EX4.BIKE$AvgHumidity), 
                       AvgWindspeed = mean(EX4.BIKE$AvgWindspeed))
averages
EX4.BIKE[337, ]


bike_result_new <- lm(Demand ~ ., data = EX4.BIKE[-337, ])
summary(bike_result_new)

# model with all 2 way interactions
bike_result_2way <- lm(Demand ~.^2, data = EX4.BIKE[-337, ])
see_interactions(bike_result_2way, many = TRUE)
summary(bike_result_2way)

final_bike <- lm(Demand ~ AvgTemp + AvgWindspeed + AvgTemp*AvgWindspeed, data = EX4.BIKE[-337, ])
summary(final_bike)
visualize_model(final_bike)


data(STUDENT)
head(STUDENT)

student_result <- lm(CollegeGPA ~ JobHours + HSGPA + JobHours*HSGPA, data = STUDENT)
summary(student_result)
see_interactions(student_result)


act_result <- lm(ACT ~ HSGPA + Honors + HSClubs + HSGPA*Honors*HSClubs, data = STUDENT)
summary(act_result)

result_simple <- lm(ACT ~ HSGPA + Honors + HSClubs, data = STUDENT)
result_complex <- lm(ACT ~ HSGPA + Honors + HSClubs + HSGPA*Honors + 
                       HSGPA*HSClubs +
                       Honors*HSClubs, 
                     data = STUDENT)
anova(result_simple, result_complex)


act_final <- lm(ACT ~ HSGPA + Honors, data = STUDENT)
summary(act_final)
check_regression(act_final)


