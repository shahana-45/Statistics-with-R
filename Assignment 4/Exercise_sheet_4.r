### Stats with R Exercise sheet 4

##########################
# Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 28. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates') name, matriculation number. 
## Name:
## Matriculation number:

###########################################################################################
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and t tests
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## NativeLanguage (native language of participant)
## RT (log reaction time)
## Sex (of the participant)
## PrevType (whether the preceding word was a real word or a nonword)
## Class (whether the target word was denoting an animal or a plant)



## 1. Create the dataset lexdat, which is a copy of lexdec, but only includes the columns 
##  indicated above
lexdat = lexdec[c("Subject", "NativeLanguage", "RT", "Sex", "PrevType", "Class")]
head(lexdat)

## Say you are interested in the influence of the previous word type on lexical decision time.
## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by prevType, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI.
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}

## 2. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by prevType and get the mean as well as the
##  se of RT. Store the result to summaryByPrevType
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
summaryByPrevType <- lexdat %>%
  group_by(PrevType) %>%
  summarize(mean = mean(RT, na.rm = TRUE), sd = sd(RT))

summaryByPrevType


## 3. Describe the resulting data set (summaryByPrevType) in your own words
# The summaryByPrevType data frame contains the mean reaction times that the participants took to decide whether
# what they were shown is a word or non word, as well as the standard deviations from the means.

## 4. Now use summaryByPrevType to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByPrevType) +
  geom_bar( aes(x=PrevType, y=mean), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=PrevType, ymin=mean-(1.96 * sd), ymax=mean+(1.96*sd)), width=0.4, colour="orange")


## 5. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByPrevType, aes(x=PrevType, y=mean, group=1, color=PrevType)) + 
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean-(1.96 * sd), ymax = mean+(1.96*sd)))

## 6. Gauging from the plot, does it look like there's an important difference in mean RT 
##  after words compared to nonwords?

# From the plot, It looks like the mean reaction time for words is slightly less than that of non-words. 

## 7. Let's go back to the original data frame "lexdat".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT after words vs nonwords.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?

# The independence assumption is violated, as the same participant provides 79 responses 
# and hence there are repeated measures. Hence, there is correlation in the data.

## 8. We need to restructure the data to only one observation (average RT) per subject 
##  and word/nonword condition (PrevType). We will again use group_by and summarize, but
##  this time we have to group by Subject and PrevType, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj

bySubj <- lexdat %>%
  group_by(Subject, PrevType) %>%
  summarize(mean = mean(RT, na.rm = TRUE))

bySubj

## 9. Create histograms of the RT data in bySubj depending on the preceding word 
##  type and display them side by side. Set the binwidth to 0.08
par(mfrow=c(1,2))

h1 <- filter(bySubj, PrevType == "word")
h2 <- filter(bySubj, PrevType == "nonword")

hist(h1$mean, breaks = 8, main="Word")
hist(h2$mean,breaks = 8, main="Non-word")

## 10. Display the same data in density plots. 
plot(density(h1$mean))
plot(density(h2$mean))

## 11. Based on the histograms and the density plots - are these data normally 
##  distributed?

## Yes, but the data are plotted with a slight skew.

## 12. Create boxplots of the mean RT in bySubj by PrevType

boxplot(mean~PrevType, data=bySubj)

## 13. Compute the t-test to compare the mean RT between decisions following on a word
##  vs a nonword using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?

t.test(mean ~ PrevType, data = bySubj, paired = TRUE)

## Since the data are not independent, we required a paired t-test.

## 14. What does the output tell you? What conclusions do you draw?

## According to the output of the t-test, p < 0.05. Hence, we can reject the null hypothesis.

## 15. In addition to the long-format data we've just been working on, you may also 
## encounter data sets in a wide format (this is the format we have been using in 
## class examples.)
## Let's look at a different variable, namely the semantic class (Class) of the target 
## word instead of the type of the previous word. Again, summarize the dataset
## to obtain the mean RT by subject and class and transform the dataset to a 
## wide format. In addition to group_by() and summarize(), you will need the function 
## spread(). Assign the result to wide

#DOUBT

wide <- lexdat %>%
  group_by(Subject, Class) %>%
  summarize(mean = mean(RT, na.rm = TRUE)) %>%
  spread(lexdat, key = Class, value = mean)

## 16. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()

t.test(wide$animal, wide$plant, paired = TRUE)

## 17. What do you conclude from this?

#TODO

## 18. Now let's look at yet another question, namely whether the native language 
##  of the participant influences their reaction time. Check out the variable
##  NativeLanguage. Can you use a t-test to pursue this question and which type
##  of t-test would you use? Can you think of a situation, where a t-test would not 
##  be enough to test for a difference depending on the native language?

lexdat$NativeLanguage

## Yes, a t-test can be performed and the independent t-test can be used since there are two 
## independent categories - English and other.

#TODO

## 19. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to NativeLanguage and assign it to bySubjNatLang
## Perform the t-test you decided for.

bySubjNatLang <- lexdat %>%
  group_by(Subject, NativeLanguage) %>%
  summarize(mean = mean(RT, na.rm = TRUE))

t.test(mean ~ NativeLanguage, data = bySubjNatLang)

## 20. What do you conclude?

## From the results of the t-test, since p < 0.05, we can reject the null hypothesis and
##conclude that the native language does influence the reaction time of a person.

## 21. Compute the effect size using Cohen's D. 

## 22.  Which effect size do we get? How do you interpret this result?

## 23. Choose an appropriate plot to visualize the difference between group


###############
### 2. T-Test
###############
## In this exercise we will try to explore the independent samples t-test 
## and its affect on different samples. 
## We will take the same example discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(8254)
## 1. Generate 15 samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"

## 2. Now we generate our second sample of size 15, this time for tutor 2 and with mean 35 and 
## sd 15

## 3. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)

## 4. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 15 times followed by "tutor2" 15 times


## 5. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.

## 6. Run the independent samples TTest (independentSamplesTTest()) and formulate the findings as discussed 
###  in the lecture. 
##	independentSamplesTTest() also provides the effect size (Cohen's d). How do you interpret the effect size?

## 7. Time to play around!
##	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
##	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
##	what do you observe when we keep the means and sd same?
