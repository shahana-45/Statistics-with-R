### Stats with R Exercise sheet 7

#############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
#############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 19. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 

## Team: 31
## Name:Senorita Rodricks
## Matriculation number: 7015445
## Name:Shahana Mogal
## Matriculation number: 7015588
## Name:Sneha Mariam Sam
## Matriculation number: 7015831

###############################################################################
###############################################################################

## The following line of code clears your workspace.

rm(list = ls())


#############################################################################
### Exercise 1
#############################################################################

#############################################################################
### Please, use ggplot to make plots in all exercises unless specified differently!
#############################################################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 4, we ran a t-test to test for differences in RT after a word vs after a non word.
##  In sheet 5, we looked at correlations between RT and frequency and length. In this sheet we will 
##  combine these analyses, look for interactions and again look at model assumptions
##  and model diagnostics

## a) Load the dataset lexdec from package languageR and store it in a variable called data

library(languageR)
data <- lexdec

## b) Run a simple regression, just including Frequency as predictor and RT as the dependent variable
##  Store it in lm1

lm1 <- lm(Frequency ~ RT, data=data)

## c) Report and explain the effect of Frequency

summary(lm1)

## According to the p-value of the lm summary, we see that it is less than 0.05,
## which means that the null hypothesis can be rejected - meaning that there is a significant
## relationship between the frequency of the word and the reaction time.

## d) Make a scatterplot of RT by Frequency, including the regression line

library(ggplot2)

ggplot(data, aes(x=RT, y=Frequency)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

## e) Next, fit a model including Frequency and PrevType as predictors, store it in lm2

## f) Report and explain the effects of Frequency and PrevType.

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
 data$RT_pred = fitted(lm2)

## g) Now, plot the original data (RT by Frequency with different colors for PrevType), but use the 
## fitted values (RT_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.

## h) Run a regression model that includes also the interaction between Frequency and PrevType and store it
##  as lm3

## i) Plot the results of the model! (This time no need to specify the pred data set)

## j) Report the results of lm3 and interpret with the help of the graph in i)

## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

## l) Interpret what you see in k) and possibly suggest further steps

## m) So, what assumptions are violated in the model as it is? Consider both your results from l and what you know 
##  about the data set from previous analyses.
