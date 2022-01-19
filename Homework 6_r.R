library(regclass)

data(EX5.BIKE)
head(EX5.BIKE)
str(EX5.BIKE)

associate(EX5.BIKE$Demand ~ EX5.BIKE$Weather)

result <- lm(Demand ~ EffectiveAvgTemp + Weather, data = EX5.BIKE)
summary(result) # Y = 1912.933 + 125.521x1 - 656.097
visualize_model(result)

result_2 <- lm(Demand ~ EffectiveAvgTemp + Day, data = EX5.BIKE)
summary(result_2)
visualize_model(result_2)

result_3 <- lm(Demand ~ ., data = EX5.BIKE)
summary(result_3)

drop1(result_3, test = 'F')

data(EX5.DONOR)
head(EX5.DONOR)

result_4 <- lm(LastAmount ~ RecentAvgAmount + Homeowner + RecentAvgAmount*Homeowner, data = EX5.DONOR)
summary(result_4)
visualize_model(result_4)

result_5 <- lm(LastAmount ~ RecentAvgAmount + AccountAge + Setting + RecentAvgAmount*Setting, data = EX5.DONOR)
summary(result_5)
drop1(result_5, test = 'F')
