library(regclass)

# Association - analysis of relationship between 2 variables
#   1) Visualize the data (look at the distribution i.e. graphs, tables)
#   2) Numerically measure the strength of association <- compute statistics
#   3) Determine whether association is stastically significant

# Categorical Data
#   - Frequency and relative frequency tables (bar charts, pie charts)
#   - CALLS dataset


data(CALLS) # loading in data from CALLS dataset
head(CALLS) # reading first 6 observations 

table(CALLS$DropCallFreq) # frequency table of dropcallfrequency
table(CALLS$DropCallFreq) / length(CALLS$DropCallFreq) # relative frequency table

table(CALLS) # creating a table of CALLS (joint distribution)

tbl <- table(CALLS)

segmented_barchart(CALLS$DropCallFreq) # segmented barchart of dropcallfreq
segmented_barchart(CALLS$Provider) # segmented barchart of providers

barplot(tbl, legend = TRUE) # barplot of our table


# Quantitative Data
#   - mean, standard deviation, five number summary, median, etc. (histogram, boxplot)


data(STUDENT) # loading in data from STUDENT dataset
head(STUDENT) # reading first 6 observations

hist(STUDENT$CollegeGPA) # histogram of collegegpa
boxplot(STUDENT$CollegeGPA) #boxplot of collegegpa
boxplot(STUDENT$CollegeGPA ~ Gender, data = STUDENT) # difference between gender for collegegpa

summary(STUDENT$CollegeGPA) # five number summary + mean for collegegpa
sd(STUDENT$CollegeGPA) # standard deviation for collegegpa

qqnorm(STUDENT$CollegeGPA) # q-q plot used to check normality
qqline(STUDENT$CollegeGPA) # adding a line to determine normality

qq(STUDENT$CollegeGPA) # q-q plot using the regclass package 

hist(STUDENT$HSGPA) #  histogram of high school gpa
qq(STUDENT$HSGPA) # q-q plot of high school gpa


# Associations when X and Y are both CATEGORICAL


associate(DropCallFreq~Provider, data = CALLS) # use formula notation Y ~ X, data = 

## WHAT HAVE WE LEARNED? (CALLS)
#   - most students have Verizon (thickest bar) <- little over 50%
#   - AT&T was next with approximately 30% 
#   - overall ~ 75% of students rarely had dropped calls, but
#       - Verizon ~ 90%
#       - Sprint ~ 75%
#       - U.S. Cellular ~ 70%
#       - AT&T ~ 55% 
#   - VERY DIFFERENT - we have an association (would have been similar if no association)

# test whether associations are statistically significant 
#   - quantify the difference between the conditional and marginal distributions (statistic)
#       1) compute frequencies/percents we'd have expected [E] with no association
#       2) compute a type of difference measure between expected and observed [O] values


# HYPOTHESIS TEST
#   1) Establish the size of the difference (what values of the statistic) we'd expect to see
#      by chance if there's no association 
#   2) Compare to observed statistic and if a difference at least as large as the one observed
#      occurs less than 5% of the time, we can conclude that the association is statistically
#      significant 

# Computing the statistic for the chi-square test
#   1) Find the frequency for each provider that would match the marginal distribution of Y
#         - If there is no association, there we'd expect ATT users to say occ. 18.1% of the time
#           173(0.181) = 31.313
#         - If no association, then 6.4% would say often  173(0.064) = 11.072
#         - If no association, then 75.5% would say rarely 173(0.755) - 130.615

# raw total x (cof total/n)


# Sprint = 41 students
#   - If no association, then:
#   - E = occ = 41(0.181) = 7.42
#   -     often 41(0.0.064) = 2.624
#   -     rarely 41(0.755) = 30.955

# All E in 2nd table of R output

# Quantify difference between E and O:  (O - E)^2 / E | E = expect, O = observed

# DO THIS FOR ALL PROVIDERS
#       - statistic D = x^2 (D = discrepancy)

# Is "D" large enough that we can say there is a significant association?
#       - If no association, what should D = ? ZERO!
#       - We don't expect D to always = 0 (could be larger due to random chance)
#       - Need to know what D can equal by random chance if no association


# PERMUTATION TEST
#   - Assume x and y are independent (not associated)
#   - Provider has no affect on drop call frequency, so we can randomly redistributed
#     the data for Y - re-assigning (permutation) it to the students without any consideration
#     for which provider a student has
#   - Compute D for each permutation
#       - After a ton of permutations, we'll have a really good idea of what D should be
#         if there is no association (CALLED A SAMPLING DISTRIBUTION)


# importing library
library(regclass)

# importing dataset
data(CALLS)
head(CALLS)

# checking association our data
associate(DropCallFreq~Provider, data = CALLS)

# If there is no association, D should be between 0 and 20, not as large as 78.655
# Therefore, we can rule out the possibility that there is no association
#   - CONCLUDE - there is a statistically significant association between provider
#                and dropped call frequency

# HOW TO COMPUTE P-VALUE
#   count # of permutations where D = 78.655 or is larger
#   divide by # of permutations

# INTERPRETING P-VALUE
#   If there's no association, then there's a 0% chance that we'd observe D = 78.655 (or larger)

# GENERAL RULE <- Reject "no association" if p-value < 0.05

# importing SURVEY11 dataset
data(SURVEY11)
head(SURVEY11)

# Y = weapon
# X = Gender
# Formula notation: Y~X
associate(X36.WeaponAttractMate ~ X02.Gender, data = SURVEY11)


# REJECTING P-VALUES
#   If p-value = 0.045 <- < 0.05 - reject "no association"
#   CI : (0.03, 0.06) - p-value could be as large as 0.06, so we still reject "no association" 
#     - Increase number of permutations

associate(X36.WeaponAttractMate ~ X02.Gender, data = SURVEY11, permutations = 1000)


# CLASSICAL RESULT
#   - ex: Relationship between a person's relationship status and whether the person smokes?

library(regclass)
data(SURVEY11)
head(SURVEY11)


associate(X34.RelationshipStatus ~ X42.Smoker, data = SURVEY11, permutations = 3000)


# null distribution (sampling distribution)

# using actual formula to compute p-value
#     - find probability that D is larger than 7

# P(D > 7) = integration 7 to infinity of chi-square function


## HW - Chapter 2 #1, 2

# Using SURVEY11 data set choose 2 categorical variables and test for an association



# 2.4 Association when Y is quantitative and X is categorical

# ex) woman's characteristics (as rated by others) is associated with whether she is smiling
#         - data ATTRACTF

data(ATTRACTF)
head(ATTRACTF)

# make box plots to compare scores
#   - can also can compare histograms 

boxplot(Score~Smile, data = ATTRACTF)

# Add histograms

# Strategy - assess whether there's a significant difference between mean (or median) values
#            of Y for different levels of x

# associate function can still be used!
# on boxplot = mean

# ANOVA - analysis of variance
#   - compares means
#   - statistic for ANOVA <- F = measure of variance between group means/ measure of variation within groups
#   - F is a ratio
#       - F = 3.2 means that the observed variability in group means is 3.2 times higher than we'd
#             expect to see by chance
#   - If there is no association, F = 1
#   - Larger values mean there is an association 

associate(Score~Smile, data = ATTRACTF)


boxplot(Score~Glasses, data = ATTRACTF)

associate(Score~Glasses, data = ATTRACTF)





