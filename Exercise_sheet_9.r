####################################################
#Week 11: Model Families and Logistic Regression
####################################################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 

## Team #31

## Name:Senorita Rodricks
## Matriculation number: 7015445

## Name:Shahana Mogal
## Matriculation number: 7015588

## Name:Sneha Mariam Sam
## Matriculation number: 7015831

##################################################################################

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.

## a) Build a simple logistic regression model that models the probability of survival (binary) based on 
##  sex (categorical) and passengerClass (categorical) without an interaction and store it in mSurv. 
##  You have to use the glm() function and specify the family correctly.

mSurv <- glm(survived ~ sex + passengerClass, data = TitanicSurvival, family=binomial)

## b) Look at the summary. What group does the intercept correspond to?

summary(mSurv)

## The intercept indicates the log of odds of the people who survived in the Titanic disaster
## without considering any predictors.

## c) Were men more likely to survive than women? Is the effect significant?

## No, men were less likely to survive than women.
## Yes, the effect is significant since the p-value is less than 0.05.

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##  Calculate their expected survival on the logit scale (i.e. the scale of the model) either by hand or 
##  using predict() with a new data.frame

new_data <- data.frame(sex=c("female", "male"), passengerClass=c("1st", "3rd"))

prediction <- predict(mSurv, new_data)
prediction

## e) Transform your results from d) to the probability scale, using the formula given on the slides. 
##  You can check your calculation by asserting the probabilities lie in the 0-1 range. For whom does 
##  the model predict the higher probability of survival?

pred_probability <- 1/(1+exp(-prediction))
pred_probability

## The model predicts higher survival rates for Rose (89.17%).

##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption and sleep (among others). 
## The data set "coffee.csv" contains data from 10 students, who reported on 10 randomly chosen days of the year: 
##  sleep: how many hours of sleep they had in the previous night
##  mood: how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
##  In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they are in a bad mood.
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat <- read.csv("coffee.csv", header = TRUE)


## b) Plot the number of consumed cups of coffee in three individual scatterplots by sleep, mood, and temperature. 
##  You can use geom_jitter() to get a nicer plot
ggplot(coffeedat) +
  geom_jitter( aes(x=coffee, y=sleep))

ggplot(coffeedat) +
  geom_jitter( aes(x=coffee, y=mood))

ggplot(coffeedat) +
  geom_jitter( aes(x=coffee, y=temperature))

## c) Can you detect an obvious relationship in any of the plots? Which direction does it have?

# There seems to be a relationship between sleep and coffee and it has a negative slope/direction.


## d) fit a simple linear regression model with all three predictors and store it in linmod
linmod <- lm(coffee~sleep+mood+temperature, data=coffeedat)


## e) fit a generalized linear model with the appropriate family (hint: coffee is a count variable) and
##  store it in poimod

poimod <- glmer(coffee ~ mood + sleep + temperature  + (1+sleep|mood) + (1+temperature|mood) , data=coffeedat, family=poisson)

## f) Look at the two summaries, what changed?
summary(linmod)
summary(poimod)

## In the simple linear regression model, sleep and temperature did not have a significant effect on
## the number of cups of coffee consumed. However, in the generalized linear model, temperature
## has a significant effect on the amount of coffee consumed.

## g) In fact, we have repeated measures in our design, so refit the model including a random intercept for
##  subject using glmer() with the correct family specification and store it in mixedpoi

## h) Look at the summary and report what changed in comparison to both linmod and poimod.

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin

## j) Compare the AIC for all four models. Which one has the best fit?

## k) And which model is conceptually the appropriate one? Why?

## l) Finally, report on the effects of interest in light of our research hypotheses specified above for the 
##  model you chose in k)
