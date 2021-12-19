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

lm1 <- lm(RT ~ Frequency, data=data)

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
lm2 <- lm(RT ~ Frequency+PrevType, data=data)

summary(lm2)

## f) Report and explain the effects of Frequency and PrevType.

## According to the p-value of the lm summary, we see that it is less than 0.05,
## which means that the null hypothesis can be rejected - i.e. there is a significant
## relationship between the frequency of the word and the reaction time. The same can be 
## said of PrevType.

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
data$RT_pred = fitted(lm2)

## g) Now, plot the original data (RT by Frequency with different colors for PrevType), but use the 
## fitted values (RT_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.

ggplot(data, aes(x=RT, y=Frequency, color=PrevType)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, data=data)

## h) Run a regression model that includes also the interaction between Frequency and PrevType and store it
##  as lm3

lm3 <- lm(RT ~ Frequency*PrevType, data=data)

## i) Plot the results of the model! (This time no need to specify the pred data set)

plot(lm3)

## j) Report the results of lm3 and interpret with the help of the graph in i)
# We get 4 plots with lm3 as below :
#i) Residual vs fitted: We can notice the homogeneity of the variance in the plot i.e. the residuals are equally distributed 
#ii) Normal QQ plot: It would be a perfect normal distribution if all the points would lie on the dotted line, 
#    but points 1194, 1619,1620 make the residual's distribution slightly skewed.
#iii) Fitted value vs Scale Location : Residuals are spread equally along the ranges of predictors as the line is 
#  rougly straight (horizontal)
#iv) Residual vs leverage : points 1619, 1194, 1187 have very high leverage and large standardized residuals.


## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

par(mfcol=c(2,3))
plot(lm3 <- lm(RT ~ Frequency*PrevType, data=data), which=seq(1,6))

#Adding to the plot descriptions from above:
# v) Cooks distance : Shows that the model would change drastically if we remove points 1194, 1619, 1187
# vi) Cooks distance vs leverage : Shows us how much leverage our graph has wrt data points

## l) Interpret what you see in k) and possibly suggest further steps
# We can perform various tests like Levene's test  (to determine if the variance is homogeneous or not
# and check if the values are significantly independent or not), 
# Kruskall's Wallis test (to check normality of residuals) or 
# calculate VIF (to check correlation between predictors)

## m) So, what assumptions are violated in the model as it is? Consider both your results from l and what you know 
##  about the data set from previous analyses.
 leveneTest(RT ~ Frequency * PrevType, data=data)
 #p-values are significant, therefore variance is homogeneous
 
 kruskal.test(RT ~ Frequency, data=data) 
 kruskal.test(RT ~ PrevType, data=data)
#Both p-values are significant, therefore residuals follow a normal distribution
 
 vif(lm3)
# The correlation between the predictors is causing a lot of uncertainty, 
# therefore the assumption about the correlation between the predictors is violated.
