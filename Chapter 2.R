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









