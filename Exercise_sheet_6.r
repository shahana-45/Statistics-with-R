### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 12. Write the code below the questions. 
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

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(dplyr)
library(car)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

## a) For the further reference please use ?amis. 
## It may take some time to understand the dataset. 

?amis

## b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.

data <- amis
glimpse(data)
t.test(speed~warning, data=data)

## c) All our columns have numeric type. Convert the categorical columns to factors.

data$warning <- factor(data$warning)
data$period <- factor(data$period)
data$pair <- factor(data$pair)

str(data)

## d) Plot boxplots for the distribution of `speed` for each of the `period` values 
## (before, immediately after and after some time). Build 2 plots (each containing 3 
## boxplots) side by side depending on the `warning` variable.
## (For all plots here and below please use ggplot)

install.packages('patchwork') #because par() does not work with ggplot2 package
library(patchwork)

newdata <- filter(data, warning==1)

plot1 <- ggplot(newdata) +
  geom_boxplot(aes(period, speed))

newdata2 <- filter(data, warning==2)
plot2 <- ggplot(newdata2) +
  geom_boxplot(aes(period, speed))

plot1 + plot2

## e) What can you conclude looking at the plots? What can you say about people's 
## behaviour in different periods: before, immediately after and after some time?

## Looking at the plots, we see that there was not much of a change between the period before
## erecting the warning signs and immediately after erecting the warning signs. But after a while,
## it seems that the speed has been increasing.

## f) What are your ideas about why the data with warning==2 (sites where no sign was 
## erected) was collected?

## For places without a warning sign (warning==2), the speed has been relatively higher compared
## to the locations with the warning sign, i.e. the people have become more aware of reducing their
## speed when seeing a warning sign.

#######################
### Exercise 2: 1-way ANOVA
#######################

## a) First let's create a new data frame which will be used for all exercise 2.
## For the 1-way ANOVA we will be working with a subset of `amis` using only the 
## data for sites where warning signs were erected, which corresponds to warning==1. 
## Therefore first subset your data to filter out warning==2 and then apply group_by() and summarize() 
## to average "speed" over each "pair" and "period". 
## Assign this new data frame to the variable casted_data.

data_sub <- subset(amis, warning==1, select=c(speed, period, pair, warning))

casted_data <- data_sub %>%
  group_by(pair, period) %>%
  summarize(mean = mean(speed, na.rm = TRUE))
casted_data

## b) Build boxplots of the average speed depending on "period".

ggplot(casted_data) +
  geom_boxplot(aes(period, mean, group=period))

## c) Looking at the boxplots, is there a difference between the periods?

# Yes, both the ranges as well as the means for the periods differ. Also, there is an 
# outlier for period #2.

## d) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
## speed depending on the period and assign the result to aov1way
aov1way <- aov(mean ~ period, data = casted_data)

## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why.

## e) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)

# There is no formal test to verify the independence assumption. In order to check whether 
# it is satisfied, the data collection method needs to be analysed. From the data collection,
# it is obvious that each sample is independent of the other and hence there is independence
# of observations within groups. Similarly, there is independence between groups.

## f) Normality of residuals
##  First add the residuals to your casted data set, you find them in model$residuals
##  next, make a qqplot (using qqnorm() or geom_qq() ina ggplot) for the residuals and 
##  run the shapiro wilk test.

casted_data$Residuals<-aov1way$residuals

qqnorm(casted_data$Residuals)
qqline(casted_data$Residuals)

shapiro.test(casted_data$Residuals)


## g) What do you conclude from your results in f?

# From the shapiro-wilk normality test, we can see that the p-value is 0.01798 which
# is sufficiently large, which is greater than the alpha of 0.05. Hence, the null hypothesis is
# accepted which states that the data is normally distributed. From the Q-Q plot, we can say the same.


## h) Homogeneity of variance of residuals
##  First, plot the residuals by period (boxplots) to see whether variance differs between groups
##  Next, run Levene's test using the function leveneTest() (from library car) with the same syntax
##  as aov(). It indicates whether the variance is significantly different between groups (= not
##  homogeneous).

ggplot(casted_data) +
  geom_boxplot(aes(period, Residuals, group=period))


leveneTest(casted_data$mean, casted_data$period)

## i) What do you conclude from your results in h?

# The p-value for the Levene's test is 0.8383, which is larger than that of the alpha (0.05). 
# This means that the null hypothesis can't be rejected (null hyp. is that the variance betwee
# groups is same).


## j) Now we turn to the results. Look at the summary of aov1way
summary(aov1way)

## k) State your conclusion
#Instead of getting information for each coefficient, we get information for each factor. 
#Here in this case, we have the summary for the factor 'period' and the residuals (i.e. 
#difference between mean and entered value)

## l) Please do pairwise t-tests of the same variables as in d) using pairwise.t.test().
pairwise.t.test(casted_data$mean, casted_data$period)

## m) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
pairwise.t.test(casted_data$mean, casted_data$period,p.adjust.method = "none")

pairwise.t.test(casted_data$mean, casted_data$period,p.adjust.method = "bonferroni")

## n) If the results change  in m, why do they? What does Bonferroni correction do?
# The results change in m because because the 2 test are run without and with correction
# (Bonferroni) respectively.  
# In the Bonferroni correction, p-values are multiplied by the number of comparisons.


#######################
### Exercise 3: 2-way ANOVA
#######################
## a) Now we want to analyze the influence of 2 categorical variables 
## (period and warning) on the speed.
## So let's turn back to our initial dataset amis (not its subset with warning==1).
## First, we need to average the speed over each `pair`, `warning` and `period
## Cast your data again and assign the results to casted_data2.
casted_data2 <- amis %>%
  group_by(pair,warning, period) %>%
  summarize(mean = mean(speed, na.rm = TRUE))


## b) State the main difference between the applicability of 1-way and 2-way ANOVA.
#The main difference between the applicability of 1-way and 2-way ANOVA is that 
#in 1-way ANOVA there is only 1 independent variable used  and in 2-way ANOVA there are
#multiple independent variables used.


## c) Do you think, we need to include an interaction term in the ANOVA?
#No we don't need to include an interaction term in the ANOVA 
#because the 2 independent variables are not dependent on each other

## e) Now apply the 2-way ANOVA: please use the function aov() with mean speed as the
## dependent variable, period and warning as predictor (independent) variables and depending on your
## answer in c) either including an interaction term, or not.
aov2way <- aov(mean ~ period + warning, data = casted_data2)

## f) Report the p-values and interpret the results in detail. Properly formulate the findings
##  with regard to the research question!

summary(aov2way)[[1]][["Pr(>F)"]][1]

#p value = 0.1808576
#Since the p-value is above 0.05, we can accept the null hypothesis
#As we accept the null hypothesis, we can conclude that the warning signs don't affect the speed 
#(this can also be seen in the box plots in ans 1.d)
