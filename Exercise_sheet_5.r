### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 5. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Please use the ggplot2 library for all graphs in this homework.


## Please write below your (and your teammates') name, matriculation number. 

## Team: 31
## Name:Senorita Rodricks
## Matriculation number: 7015445
## Name:Shahana Mogal
## Matriculation number: 7015588
## Name:Sneha Mariam Sam
## Matriculation number: 7015831

###########################################################################################
###########################################################################################

library(languageR)
library(ggplot2)
library(dplyr)

#######################
### Exercise 1: Correlation
#######################

## a) We will again use lexdec from languageR. This week, we will look at the variables 
##  RT, FreqSingular, Trial, Frequency, and Length. Create a dataset called data with just
##  those variables

data <- lexdec[c("RT", "FreqSingular", "Trial", "Frequency", "Length")]

## b) Take a look at the data frame.

head(data)

## c) Let's say you're interested in whether there is a linear relationship between 
## Frequency and Length 
## Take a look at the relationship between the frequency and length by 
## means of a scatterplot (use the ggplot library for this).

ggplot(data, aes(x=Frequency, y=Length)) + 
  geom_point()

## d) Judging from the graph, do you think that word frequency and word length are 
## in any way correlated with one another?

## From the graph it seems that there is a very slight correlation in the negative direction
## between the frequency and length of the word.

## e) We are also interested in correlations between RT and all other variables in
## data. Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. Is the correlation between
## Frequency and Length like you expected?

cor(data, use="complete.obs")

## Yes, the correlation value between Frequency and Length is -0.429, which was as expected.

## f) What is the range of a correlation, what is the meaning of a correlation of 0,
## 1, and -1 respectively?

## The range of a correlation is from -1 to 1.
## When correlation = 0, it means that there is absolutely no correlation between the 2 variables,
## When correlation = 1, it means that the two variables have a positive correlation, i.e., when the
## value of one variable increases, the value of the other also increases.
## When correlation = -1, it means that the two variables have a negative correlation, i.e., when the
## value of one variable increases, the value of the other decreases.

## g) Going back to the correlation matrix obtained above, please describe the correlations
##  between RT and the other variables in words.

## RT seems to have a slight negative correlation with all the other variables in data, except
## with the variable Length. As the length of the word increases, the reaction time also increases.

## h) Is the correlation between RT and FreqSingular significant? Use cor.test()

cor.test(x=RT, y=FreqSingular, data=data)

## From the results obtained, since the p-value < 0.05, we can conclude that the
## null hypothesis is rejected and that there is a significant correlation between
## RT and FreqSingular.

## i) Calculate the Spearman rank correlation between RT and FreqSingular. 
## What do you observe?

cor.test(x=RT, y=FreqSingular, data=data, method = "spearman")

## Here, the p-value is still <0.05 which means there is a significant correlation between
## RT and FreqSingular.Also the correlation value, rho is -0.228 compared to -0.139 in the
## previous output, which indicates a stronger correlation in the negative direction
## between the two variables.

## j) To get a better overview, we will create a plot between FreqSingular and 
## the mean RT per FreqSingular level.  
## Use group_by() and summarize() to obtain mean RTs by FreqSingular.
## Make a scatter plot between the two variables of the resulting data set.

## k) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?

## l) Calculate Kendall's tau for the same relationship. 

## m) Is there any reason to prefer this correlation measure in the current context? 
##  In general, in what contexts would you use Kendall's tau?

################################
### Exercise 2: Regression
################################


## a) Read in the data set lexicalDecision2.csv provided on cms and turn the variable Word 
##  into a factor. This data set is similar to the one used above in that it looks at lexical decision 
##  times for different words with the explanatory variables Frequency, Length and SynsetCount.

## b) First, we will investigate the relationship between meanRT and Length, which gives the length 
##  of the word in letters. Make a scatter plot of meanRT and Length (as always: ggplot). You can use
##  geom_jitter() to avoid overplotting

## c) Run a regression model with meanRT and Length and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
## "summary(modelname)"

## d) Interpret the model from c. What do intercept and the coefficient of Length tell you?

## e) What about the model fit: What proportion of the total variance is explained by your model?

## f) Now let's turn to the relationship between meanRT and Frequency. Run the regression and 
## interpret.

## g) Plot meanRT by Frequency and add a regression line to your plot

## h) Redo the plot, but instead of points, plot the Word value.
## Do you think there are any "bad" outliers, i.e. highly influential data points in your data set? 

## i) Rerun the model excluding the data point for the word "egplant". Compare the results.

## j) Given the difference between the two models and the peculiarities that you observe for this 
## data point, would you exclude this data point from further analysis?


###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of Frequency and Length
## on meanRT simultaneously. 

## a) Run a multiple regression model with Frequency and Length as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"

## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?

## c) Compare to the model in 2c (only including Length), has the model fit improved? How about
## the model in 2f (only including Frequency)?

## d) Using the model from 3 a: What is the predicted meanRT for the word "giraffe", which has a Frequency 
## of 3.33. Calculate "by hand", i.e. do not use predict() and show your calculation.
