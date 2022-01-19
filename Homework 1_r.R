data(SURVEY11)
head(SURVEY11)

# association between illegal drugs and night in jail
associate(X45.NightInJail ~ X43.IllegalDrugs, data = SURVEY11)


data(ATTRACTM)
head(ATTRACTM)

# association between man's attractiveness and smile
associate(Score ~ Smile, data = ATTRACTM)

# f statistic
anova_result <- aov(Score ~ Smile, data = ATTRACTM)
summary(anova_result)
