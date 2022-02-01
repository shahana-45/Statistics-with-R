### Stats with R Exercise sheet 7

##########################
#Week 13: Bayesian statistics 2
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, February 6. Write the code below the questions. 
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

###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


########################################
### Exercise 1
########################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 6, we ran a multiple regression model, which we will now repeat as a Bayesian 
##  analysis using the package brms.

## a) Load the dataset lexdec from package languageR and store it in a variable called data

library(languageR)
data <- lexdec

## b) Load the package brms
install.packages("brms")
library("brms")

## c) Fit a (frequentist) linear model of RT including Frequency and PrevType as predictors, store it in lm1
lm1 <- lm(RT ~ Frequency + PrevType, data=data)


## d) Fit the same model as a Bayesian regression using the function brm() and using only defaults (you don't need
##  to specify priors or fitting parameters like chains and iterations). Store it in bm1
set.seed(1111)
bm1 <- brm(RT ~ Frequency + PrevType, data=data)

## e) Look at the summaries of bm1 and lm1
summary(lm1)
summary(bm1)

## f) How do the parameter estimates compare?

# All 3 parameter estimates have very similar values (the differences are probably due to rounding errors).

## g) store the posterior samples of b_Frequency in the variable ps_freq. Use the function as_draws_df()
ps_freq <- as_draws_df(bm1)$b_Frequency
ps_freq

## h) Your colleague claims that the effect of frequency has to be smaller (meaning more negative) than -0.03.
##  What is the probability of the frequency effect being more negative than -0.03 given your posterior samples?
##  Do you agree with your colleague?
prob <- length(ps_freq[ps_freq < -0.03 ]) / length(ps_freq)
prob

# Since the probability of frequency being smaller than -0.03 is 0.997, we would agree with the colleague.

## i) Derive 95% and 80% credible intervals from ps_freq. Compare to the results above.
ci_95 <- quantile(ps_freq, probs = c(0.025, 0.975))
ci_95

ci_80 <- quantile(ps_freq, probs = c(0.1, 0.9))
ci_80

# The 95% credible interval is [-0.05200421 -0.03428475]
# The 80% credible interval is [-0.04900877 -0.03745797]
# This corresponds with the results above, as looking at both the credible intervals, we 
# can see that the range ends at less than -0.03.

## j) What is the meaning of a credible interval compared to the confidence interval in the frequentist's approach?

# A credible interval is the probability that a true value is within a range, whereas a confidence interval is the probability that a 
# given range contains the true value.

## k) Plot the model using the default function, this will give you the posteriors of the model parameters
##   as well as the trace plot, which give you an indication of the convergence of your model. The trace 
##   plot is supposed to look like a "fat hairy caterpillar", i.e. the different chains should not be 
##   separated in any part of the plot and there should not be a general pattern. Is this the case?

plot(bm1)

## l) We want the model to run quicker. Change the settings such that each chain only has 180 iterations with 1/4 of
# them as warmup. Store the result in bm2 and look at summary and trace plots. Use the provided seed to be able to 
# better compare your results (or try a different one, but provide it together with your answer!)
set.seed(1111)

bm2 <- brm(RT ~ Frequency + PrevType, data=data, iter = 180, warmup = 45)
summary(bm2)
plot(bm2)

## m) Do you think reducing the iterations was a good idea? Give reasons!

## Reducing the iterations was not a good idea. It led to poor convergence because the Rhat values
## were greater than 1.

## n) Another colleague of yours said 2 months ago to you that the effect of frequency is most likely at -0.01 +-0.005
##  Use these numbers for a normal prior of Frequency (with 0.005 as sd). Assign the model to bm3. 

bm3 <- brm(RT ~ Frequency + PrevType, data=data, prior = c(prior(normal(-0.01, 0.005),"Intercept"),
                                                           prior(normal(-0.01, 0.005),"b")))
summary(bm3)

## o) How did the estimate and credible interval of frequency change?

## The estimate as well as the range of the credible interval has increased.

## p) What class of priors does the above one belong to? 

## Since the available information is encoded to build the prior and there is a reasonable amount of data
## this belongs to the class of Principled Priors.
