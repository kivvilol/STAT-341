library(regclass)

# Is there an association between the amount of Ozone in parts per
# billion at Roosevelt Island and the month of the year?

head(airquality)
boxplot(Ozone~Month, data = airquality)

class(airquality$Month) <- 'character'

associate(Ozone~Month, data = airquality)

anova_result <- aov(Ozone~Month, data = airquality)
summary(anova_result)

associate(Ozone~Month, data = airquality, classic = TRUE) # adding classic argument

## SHAPE OF CONDITIONAL DISTRIBUTIONS
#     - ANOVA - compares means (use when all conditional distributions are well approximated 
#               by a normal distribution AND have approximately the same spread) (or when sample size large!)

#     - Kruskal-Wallis Test - ANOVA on ranks (use when there are extreme outliers which would affect the means)

#     - Median test - compares medians (use when other two do not apply, like when conditional distributions
#                                       are skewed)

## power of a test = probability of correctly detecting an association 


# Kruskal-Wallis Test example
#     - Associate between attractiveness and whether a girl is smiling

#     - rank data and then determine if the ranks are around the same

# Median Test example
#     - determine median for all scores and then decide if a score is larger or smaller than median
#     - create chart then use chi-square (x ^ 2) test



## ASSOCIATIONS WHEN Y IS CATEGORICAL AND X IS QUANTITATIVE

#     - ex) Is there an association between going to church (Y) and high school GPA (x)
#             - one strategy: group GPA values and then associate


data('STUDENT')
head(STUDENT)

associate(Churchgoer~HSGPA, data = STUDENT)


# ASSOCIATIONS WHEN Y AND X ARE BOTH QUANTITATIVE
#     - visualize using scatterplot
#         - If the stream of points is flat, feature-less, no visible pattern <- no association
#     - look at slices of graph <- conditional distribution of Y given X looks the same
#         - yes pattern <- yes association

#     - strength of association <- look at compactness of points (pay attention to direction and form)
#                                     - DIRECTION describes how variables are changing together
#                                         - Positive, Negative, non-monotonic

# FRIEND example
#   - association between how attractive someone is (as perceived by a male) and how likely a female
#     is to be friends with that person

data(FRIEND)
head(FRIEND)

plot(FriendshipPotential ~ Attractiveness, data = FRIEND)

associate(FriendshipPotential ~ Attractiveness, data = FRIEND)
# Moderate strength positive, linear association


# Numerically measure the strength of the association
#   - IDEA: compare overall variability in Y to the variability in Y once we know X
#   - If the form of the stream of points is linear, we use Pearson's correlation coefficient (r statistic)


# r is a number between -1 and +1
#   - close to + or - 1 means stream of points is very close to being a straight line 
#         - you have a strong linear association (very little vertical)
#   - close to 0 means you have a weak linear association
#   - positive value <- positive association
#   - negative value <- negative association

# EX: FRIEND
data(FRIEND)

plot(FriendshipPotential ~ Attractiveness, data = FRIEND)

associate(FriendshipPotential ~ Attractiveness, data = FRIEND)

cor(FRIEND$FriendshipPotential, FRIEND$Attractiveness)
# r = 0.747 <- moderately strong positive linear association

# r^2 = coefficient of determination
#   - tells us what percent of the overall variation in Y can be explained, or attributed to our X
#   - Informally <- knowing X gets us 100*r^2 % of the way to knowing Y
#   - FRIEND r = 0.747 <- r^2 = (0.747)^2 = 0.558
#       - Interpret: knowing how attractive a male would rate a female gets us 55.8% of the way
#                    to knowing how a female would rate the friendship potential with this person
#       - Interpret: 55.8% of the variation in friendship rating can be attributed to 
#                    how a male rates attractiveness


# EX: airquality

data(airquality)
head(airquality)

associate(Ozone~Temp, data = airquality)

# r = .77 <- r^2 = .498 <-  50% of the variation in Ozone can be attributed to Temp

associate(Ozone~Wind, data = airquality)


## SPEARMAN'S RANK CORRELATION
#     - Use if no linearity but stream of points is still similar
#     - Pearson's r computed on the ranks of the data values

#     - TEST FOR CORRELATION
#         - Even when X and Y are unrelated, r won't be exactly zero. 
#         - For the association to be statistically significant, r must be large enough(far enough from 0)
#           that it would be unlikely to occur by chance


data(FUMBLES)
head(FUMBLES)

associate(Wins~FumblesLost, data = FUMBLES)


data(DONOR)
head(DONOR)
help(DONOR)

associate(Donation.Amount ~ MEDIAN_HOUSEHOLD_INCOME, data = DONOR)

