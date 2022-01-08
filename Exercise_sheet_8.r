### Stats with R Exercise sheet 8

##########################
# Linear Mixed Effects Models
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.


## Please write below your (and your teammates) name, matriculation number. 
## Name:Senorita Rodricks
## Matriculation number: 7015445

## Name:Shahana Mogal
## Matriculation number: 7015588

## Name:Sneha Mariam Sam
## Matriculation number: 7015831


###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)
library(languageR)

#####################################################
### 1. Linear mixed model for chicken growth 
#####################################################

## a) We will first look at the dataset ChickWeight, which is already loaded in base R. Check out 
##  the help page of the data set to understand how the data was collected and look at the summary
help("ChickWeight")
summary(ChickWeight)
head(ChickWeight)

## b) Let's plot the data. We will first follow the strategy from sheet 4, i.e. 
##  1. group the data by Diet and Time and use summarise() to get the mean and se (se() as provided below)
##    of weight. Assign resulting data set to aggData
se = function(x){sd(x)/sqrt(length(x))}

aggData <- ChickWeight %>%
  group_by(Diet, Time) %>%
  summarize(mean = mean(weight, na.rm = TRUE), se = se(weight))

aggData

##  2. Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##    Also add errorbars (mean+/-1.96*se)

ggplot(aggData) +
  geom_line( aes(x=Time, y=mean), color=aggData$Diet) +
  geom_errorbar( aes(x=Time, ymin=mean-(1.96 * se), ymax=mean+(1.96 * se)))

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##  by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##  instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##  actual data

ggplot(ChickWeight) +
  geom_line( aes(x=Time, y=weight), color=ChickWeight$Chick) +
  facet_wrap(~Diet)

## d) What do you observe, looking at c?

## Looking at c, we observe that diet no. 2 and 3 have a more noticeable effect on chick weight,
## as compared to diet no. 1 and 4.

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##  looking for an interaction between time after birth and the diet type. Before running the model,
##  specify:
##  1) What fixed effect(s) do you enter into the model?

## Diet and Time are fixed effects because the observations are made at specific points of time 
## and the diet is already decided upon, hence no randomness is expected.

##  2) what random effect(s) should be included to account for the repeated measures structure of the data?

## Chick is the random effect that should be included. This is because the chicks 
## participating in the study may have different genetic compositions like faster or slower
## metabolism and thus causes a randomness.

##  3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

## TODO

## f) Run the model you specified in e) using lmer() and assign it to chickmod

chickmod <- lmer(weight ~ Diet*Time + (1|Chick), data=ChickWeight)
chickmod

## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull

chicknull <- lmer(weight ~ Diet + (1|Chick), data=ChickWeight)
chicknull

## h) compare the two models using the anova() function, which performs a likelihood ratio test

anova(chickmod, chicknull)


## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis
#P-value : 2.2e-16
#Since p-value is less than 0.05, we can conclude that the type of diet has an effect on the chick's growth


## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])
#We get the posterior uncertainty over the random effects of chickmod using the above function. 
#The plot gives us point estimates of random variables

#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set and suppose, we want to look at effects of the word type of the 
## previously presented word (each subject saw a different randomized sequence) and effects of the complexity
## of the word itself, while taking into account the dependence between data points collected on the same 
## word and from the same subject. 
## Which of the following models has a maximal random effect structure given the experimental design?
## Motivate your choice.

m1 = lmer(RT ~ PrevType+ Complex+ (PrevType|Subject) + (Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType| Word), lexdec)
m3 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType+ Complex+ (Complex|Subject) + (PrevType| Word), lexdec)
m5 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (1| Word), lexdec)

#m3 has the maximal random effect because it has the highest negative convergence value


## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##  to their final math grade in school. Performance is measured as the overall score in the final exam.
##  The summer school course has 200 participants, coming from 8 different partner Universities from all
##  over Germany. These 200 participants were randomly split into 10 tutorial groups, where each tutorial 
##  was held by a different tutor.
##  Given the design of your study, what random effects should you add to the model below?
##  Explain!!! If you want to, you can additionally add the random effects into the formula

## lmer(advancedalgebrascore ~ mathGrade + tutor + university, someData)
#We added the variables tutor and university every student will come from a different University 
#and will have a differemt tutor
