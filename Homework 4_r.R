library(regclass)

data(EX4.BIKE)
head(EX4.BIKE)
help(EX4.BIKE)

pairs(EX4.BIKE)

# simple linear regressions

result_avgtemp <- lm(Demand ~ AvgTemp, data = EX4.BIKE)
result_effavgtemp <- lm(Demand ~ EffectiveAvgTemp, data = EX4.BIKE)
result <- lm(Demand ~ AvgHumidity, data = EX4.BIKE)
result <- lm(Demand ~ AvgWindspeed, data = EX4.BIKE)

visualize_model(result_avgtemp)
visualize_model(result_effavgtemp)

summary(result_avgtemp)
summary(result_effavgtemp)
# multiple regression
result <- lm(Demand ~ AvgTemp + EffectiveAvgTemp + AvgHumidity + AvgWindspeed, data = EX4.BIKE)

summary(result)


result_simple <- lm(Demand ~ AvgHumidity + AvgWindspeed, data = EX4.BIKE)
result_complex <- lm(Demand ~ AvgTemp + EffectiveAvgTemp + AvgHumidity + AvgWindspeed, data = EX4.BIKE)

anova(result_simple, result_complex)

# determining whether to drop AvgTemp or EffectiveAvgTemp
VIF(result) # EffectiveAvgTemp is higher

check_regression(result, extra = TRUE)



data(EX4.STOCKS)
head(EX4.STOCKS)
help(EX4.STOCKS)

result <- lm(AA ~ ., data = EX4.STOCKS)
summary(result)

check_regression(result, extra = TRUE)

VIF(result)

data(EX4.STOCKPREDICT)
head(EX4.STOCKPREDICT)

# creating new dataframe

predict(result, newdata = EX4.STOCKPREDICT, interval = 'predict', level = 0.95)



















