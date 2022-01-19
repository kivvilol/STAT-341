library(regclass)

data("EX2.CENSUS")

all_correlations(EX2.CENSUS, interest = 'ResponseRate', sorted = 'strength')

r_squared_5 <- cor.test(EX2.CENSUS$HomeownerHH, EX2.CENSUS$ResponseRate, method = 'spearman')
r_squared_5

associate(EX2.CENSUS$logMedianHouseValue~EX2.CENSUS$MedianHHIncomeCity)


data('EX2.TIPS')

associate(Size_of_Party~Smoker, data = EX2.TIPS)

associate(Size_of_Party~Smoker, data = EX2.TIPS, permutations = 5000)

associate(Tip_in_USD~Bill_in_USD, data = EX2.TIPS)

associate(Smoker~Day_Night, data = EX2.TIPS)


cor_demo()

