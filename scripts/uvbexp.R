# Single Factor RCB with sub-sampling
  
# Weight gain of fish after UV exposure.

# An experiment was conducted to measure the weight gain of salmon fry
# after exposure to different combinations of UV A and B radiation. Each 
# tank had 5 fish. Groups of tanks were blocked together in flumes
# with common water flow.


uvexp <- read.csv("uvexp.csv", header=TRUE)
uvexp[1:10,]


#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average wight gain for each flume
# Use the summaryBy() function in the doBy package

library(doBy)
avg <- summaryBy(WeightGain ~ Block + Trt+ Flume , FUN=mean, data=uvexp)
avg

# Plot the data
boxplot(WeightGain.mean ~ Trt, data=avg,  main="Avg weight content for each treatment", 
        sub='Whiskers extend to range of data')
stripchart(WeightGain.mean ~ Trt, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg weight gain for each treatment", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average weight gain')


# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(WeightGain.mean ~ Trt, data=avg, FUN=c(length,mean,sd))
# SE not computed in the usual way because this is not a CRD
report



# fit the linear model and get the ANOVA table and test for effects
avg$Block <- as.factor(avg$Block)

# Trt is usually specified last in model because R gives Type I tests.
# In this case, the design is balanced so it doesn't matter.
model <- lm(WeightGain.mean ~ Block + Trt, data=avg)
anova(model)


# Estimate the marginal means.
# Use the popMeans() function in the doBy package

lsmeans <- popMeans(model, eff="Trt")
lsmeans

# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model.tukey <- glht(model, linfct = mcp(Trt = "Tukey"))
summary(model.tukey)
confint(model.tukey)

model.tukey.cld <- cld(model.tukey)  
model.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model.tukey)
plot(model.tukey.cld,  main="Multiple comparison results", 
       xlab="Trt",
       ylab="fat", 
       notch=FALSE)
par <- old.par


# Check the assumptions of the linear model on the averages
layout(matrix(1:4,2,2))
plot(model)



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
boxplot(WeightGain ~ Trt, data=uvexp,  main="WeightGain content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(WeightGain ~ Trt, data=uvexp, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg weight gain for each flume", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual weight gains')



# Fit the random effects model
# Be sure to specify that Flume and Block are a factor
uvexp$Flume <- as.factor(uvexp$Flume)
uvexp$Block <- as.factor(uvexp$Block)


library(nlme)
# Create a unique FLUME id for each combination of treatment and flume
# to avoid having to specify the nesting of flume(treatment)
uvexp$Flume.id <- interaction(uvexp$Trt,uvexp$Flume)
model2 <- lme( WeightGain ~ Block + Trt, random=~1 | Flume.id, data=uvexp)
anova(model2)



# Get the variance components
vc <- VarCorr(model2)
vc

# Get the marginal means
#Note that the marginal means cannot be computed in R, and I don't know how to fix this
popMeans(model2, effect='Trt')

# Multiple comparison 
library(multcomp)

model2.tukey <- glht(model2, linfct = mcp(Trt = "Tukey"))
summary(model2.tukey)
#Note that the confidence limits below are WRONG, and I don't know how to fix them
confint(model2.tukey)

model2.tukey.cld <- cld(model2.tukey) 
model2.tukey.cld


old.par <- par() 
layout(matrix(1:2,2,1))
plot(model2.tukey)
#Note that the Joined Letter plot isn't produced, and I don't know how to fix them

plot(model2.tukey.cld, main="Multiple comparison results", 
         xlab="Trt",
         ylab="WeightGain", 
         notch=TRUE)



#Check the residuals etc
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(model2, resid(., type = "p") ~ fitted(.) | Trt, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(model2, Trt ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(model2, WeightGain ~ fitted(.) | Trt, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(model2, ~ resid(., type = "p") | Trt, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))



#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

# Look what happens when the design is unbalanced?

# Delete some observations
uvexp2 <- uvexp[c(1,3,4,7,8,9,13:nrow(uvexp)),]
table(uvexp2$Trt, uvexp2$Flume)

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the weight gain for each FLUME
# Use the summaryBy() function in the doBy package

library(doBy)
avg <- summaryBy(WeightGain ~ Block + Trt+ Flume, FUN=mean, data=uvexp2)
avg

# Plot the data
boxplot(WeightGain.mean ~ Trt, data=avg,  main="Avg weight gain for each flume", 
        sub='Whiskers extend to range of data')
stripchart(WeightGain.mean ~ Trt, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg weight gain for each flume", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average fat')

# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(WeightGain.mean ~ Trt, data=avg, FUN=c(length,mean,sd))
# No se is computed because this is a RCB design
report





# fit the linear model and get the ANOVA table and test for effects

# We usually specify the Trt last in the model because R gives Type I SS
# Because the design is "balanced" when the averages are used, the order doesn't
# matter.
avg$Block <- as.factor(avg$Block)

model3 <- lm(WeightGain.mean ~ Block + Trt, data=avg)
anova(model3)




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

lsmeans <- popMeans(model3, eff="Trt")
lsmeans


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model.tukey <- glht(model3, linfct = mcp(Trt = "Tukey"))
summary(model.tukey)
confint(model.tukey)

model.tukey.cld <- cld(model.tukey)  # joined line plot 
model.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model.tukey)
plot(model.tukey.cld, # main="Multiple comparison results", 
       xlab="Trt",
       ylab="fat", 
       notch=TRUE)
par <- old.par


# Check the assumptions of the linear model on the averages

layout(matrix(1:4,2,2))
plot(model3)


#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
boxplot(WeightGain ~ Trt, data=uvexp2,  main="WeightGain content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(WeightGain ~ Trt, data=uvexp2, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg weight gain for each flume", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual weight gains')


# Fit the random effects model
# Be sure to specify that Flume and Block are a factor
uvexp2$Flume <- as.factor(uvexp2$Flume)
uvexp2$Block <- as.factor(uvexp2$Block)

library(nlme)
# Create a unique FLUME id for each combination of treatment and flume
# to avoid having to specify the nesting of flume(treatment)
uvexp2$Flume.id <- interaction(uvexp2$Trt,uvexp2$Flume)
model4 <- lme( WeightGain ~ Block + Trt, random=~1 | Flume.id, data=uvexp2)
anova(model4)



# Get the variance components
vc <- VarCorr(model4)
vc

# Get the marginal means
# This doesn't work with LME objects
popMeans(model4, effect='Trt')

# Multiple comparison 
library(multcomp)

model4.tukey <- glht(model4, linfct = mcp(Trt = "Tukey"))
summary(model4.tukey)
confint(model4.tukey)

model4.tukey.cld <- cld(model4.tukey)  
model4.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model4.tukey)
plot(model4.tukey.cld, main="Multiple comparison results", 
         xlab="Trt",
         ylab="WeightGain", 
         notch=TRUE)
par <- old.par



#Check the residuals etc
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(model4, resid(., type = "p") ~ fitted(.) | Trt, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(model4, Trt ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(model4, WeightGain ~ fitted(.) | Trt, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(model4, ~ resid(., type = "p") | Trt, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))



