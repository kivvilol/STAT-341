library(regclass)

data(EX3.NFL)

correlations <- all_correlations(EX3.NFL)
correlations[264:393, ]

plot(EX3.NFL$Wins, EX3.NFL$X1.Off.Tot.Yds)
results <- lm(X1.Off.Tot.Yds ~ Wins, data = EX3.NFL)
abline(results, col = 'red', lwd = 2)

summary(results)


plot(EX3.NFL$Wins, EX3.NFL$X82.Def.Fumbles.Recovered)
results2 <- lm(X82.Def.Fumbles.Recovered~Wins, data = EX3.NFL)
summary(results2)

confint(results2, level = 0.95)



data(EX3.ABALONE)
head(EX3.ABALONE)

plot(EX3.ABALONE$Meat.Weight ~ EX3.ABALONE$Diameter, pch = 20, cex = 0.3)
results3 <- lm(EX3.ABALONE$Diameter ~ EX3.ABALONE$Meat.Weight)
abline(results3, col = 'red', lwd = 2)

visualize_model(results3)
find_transformations(results3)


plot(log(EX3.ABALONE$Meat.Weight) ~ log(EX3.ABALONE$Diameter), pch = 20, cex = 0.3)
results4 <- lm(log(EX3.ABALONE$Meat.Weight) ~ log(EX3.ABALONE$Diameter))
abline(results4, col = 'red', lwd = 2)

check_regression(results4)


plot(fitted(results4), residuals(results4))


data(EX3.HOUSING)
head(EX3.HOUSING)

plot(EX3.HOUSING$PRICE, EX3.HOUSING$AREA)
results5 <- lm(AREA ~ PRICE, data = EX3.HOUSING)
abline(results5, col = 'red', lwd = 2)

# transformed plot
plot(log(EX3.HOUSING$PRICE) ~ log(EX3.HOUSING$AREA), pch = 20, cex = 0.3)



log_results5 <- lm(log(EX3.HOUSING$AREA) ~ log(EX3.HOUSING$PRICE))

visualize_model(log_results5)

check_regression(log_results5)

summary(log_results5)



data(EX3.BODYFAT)
head(EX3.BODYFAT)

tricep <- lm(Fat~Triceps, data = EX3.BODYFAT)

to_predict <- data.frame(Triceps = c(18, 31))
predict(tricep, newdata = to_predict, interval = 'confidence', level = .95)
predict(tricep, newdata = to_predict, interval = 'prediction', level = .95)

