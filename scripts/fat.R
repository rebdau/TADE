# Single Factor CRD with sub-sampling
  
# Samples of fish of 4 difference species are selected.
# From each fish of each species, three samples are taken of the muscle
# and the fat level is measured.

# Read in the data
fishfat <- read.csv("fat.csv", header=TRUE)
fishfat[1:10,]

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the fishfat for each fish
# Use the summaryBy() function in the doBy package

library(doBy)
summaryBy(Fat ~ Fish, FUN=mean, data=fishfat)# this doesn't make sense!
#first divide by species and then by fish
avg <- summaryBy(Fat ~ Species+ Fish, FUN=mean, data=fishfat)
avg

# Plot the data
boxplot(Fat.mean ~ Species, data=avg,  main="Avg fat content for each fish", 
        sub="Whiskers extend to range of data")
stripchart(Fat.mean ~ Species, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg fat for each fish", 
           sub="Whiskers extend to range of data",
           xlab="", ylab="Average fat")


# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(Fat.mean ~ Species, data=avg, FUN=c(length,mean,sd))
report$Fat.mean.se <- report$Fat.mean.sd/sqrt(report$Fat.mean.length)
report

# get the individual confidence intervals: one sample t-tests testing the mean (of means) of each species
#example:
example <- t.test(avg$Fat.mean[1:3]) 
str(example)
sapply(example,"[")#extract the elements from the list
#now for all species at once
ci <- tapply(avg$Fat.mean, avg$Species, FUN=t.test)
# use the sapply() function to extract elements from each member of the list
sapply(ci,"[","conf.int")



# fit the linear model and get the ANOVA table and test for effects
model <- lm(Fat.mean ~ Species, data=avg)
anova(model)




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

lsmeans <- popMeans(model, eff="Species")
lsmeans


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model.tukey <- glht(model, linfct = mcp(Species = "Tukey"))
summary(model.tukey)
confint(model.tukey)

# joined line plot 
model.tukey.cld <- cld(model.tukey) 
model.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model.tukey)
plot(model.tukey.cld,  #main="Multiple comparison results", 
       xlab="Species",
       ylab="fat")
par <- old.par

# Check the assumptions of the linear model on the averages
layout(matrix(1:4,2,2))
plot(model)
layout(1)


#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
boxplot(Fat ~ Species, data=fishfat,  main="Fat content for each sample", 
       sub="Whiskers extend to range of data")
stripchart(Fat ~ Species, data=fishfat, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg fat for each fish", 
       sub="Whiskers extend to range of data",
       xlab="", ylab="Individual fat concentrations")


# Fit the random effects model
# Be sure to specify that fish is a factor
fishfat$Fish <- as.factor(fishfat$Fish)

library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
fishfat$Fish.id <- interaction(fishfat$Species,fishfat$Fish)
model2 <- lme( Fat ~ Species, random=~1 | Fish.id, data=fishfat)
anova(model2)

#or using the lmerTest package
library(lmerTest)
model2b<-lmerTest::lmer(Fat ~ Species +(1|Fish.id),data=fishfat)
anova(model2b)


# Get the variance components
vc <- VarCorr(model2)
vc

# Get the marginal means
#Note that the marginal means cannot be computed in R, and I don't know how to fix this
popMeans(model2, effect="Species")

# Multiple comparison 
library(multcomp)
model2.tukey <- glht(model2, linfct = mcp(Species = "Tukey"))
summary(model2.tukey)
#Note that the confidence limits below are WRONG, and I don't know how to fix them
confint(model2.tukey)

model2.tukey.cld <- cld(model2.tukey)  # joined line plot
model2.tukey.cld


old.par <- par() 
layout(matrix(1:2,2,1))
plot(model2.tukey)

plot(model2.tukey.cld, 
         xlab="Species",
         ylab="Fat")
par <- old.par



#Check the residuals etc

# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(model2, resid(., type = "p") ~ fitted(.) | Species, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(model2, Species ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(model2, Fat ~ fitted(.) | Species, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(model2, ~ resid(., type = "p") | Species, abline = c(0, 1))

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
fishfat2 <- fishfat[c(1,3,4,7,8,9,13:nrow(fishfat)),]
table(fishfat2$Species, fishfat2$Fish)

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the fishfat for each fish
# Use the summaryBy() function in the doBy package

library(doBy)
avg <- summaryBy(Fat ~ Species+ Fish, FUN=mean, data=fishfat2)
avg[1:10,]

# Plot the data

boxplot(Fat.mean ~ Species, data=avg,  main="Avg fat content for each fish", 
        sub='Whiskers extend to range of data')
stripchart(Fat.mean ~ Species, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg fat for each fish", 
           xlab="", ylab="Average fat")



# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(Fat.mean ~ Species, data=avg, FUN=c(length,mean,sd))
report$Fat.mean.se <- report$Fat.mean.sd/sqrt(report$Fat.mean.length)
report

# get the individual confidence intervals
ci <- tapply(avg$Fat.mean, avg$Species, FUN=t.test)
# use the sapply() function to extract elements from each member of the list
sapply(ci,"[","conf.int")



# fit the linear model and get the ANOVA table and test for effects
model3 <- lm(Fat.mean ~ Species, data=avg)
anova(model3)



# Estimate the marginal means.
# Use the popMeans() function in the doBy package

lsmeans <- popMeans(model3, eff="Species")
lsmeans


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model3.tukey <- glht(model3, linfct = mcp(Species = "Tukey"))
summary(model3.tukey)
confint(model3.tukey)

model3.tukey.cld <- cld(model3.tukey)  # joined line plot 
model3.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model3.tukey)
plot(model3.tukey.cld, 
       xlab="Species",
       ylab="fat")
par <- old.par


# Check the assumptions of the linear model on the averages
layout(matrix(1:4,2,2))
plot(model3)



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
boxplot(Fat ~ Species, data=fishfat2,  main="Fat content for each sample", 
       sub='Whiskers extend to range of data')
stripchart(Fat ~ Species, data=fishfat2, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="Avg fat for each fish", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='Individual fat concentrations')



# Fit the random effects model
# Be sure to specify that fish is a factor
fishfat2$Fish <- as.factor(fishfat2$Fish)

library(nlme)
# Create a unique fish id for each combination of species and fish
# to avoid having to specify the nesting of fish(species)
fishfat2$Fish.id <- interaction(fishfat2$Species,fishfat2$Fish)
model4 <- lme( Fat ~ Species, random=~1 | Fish.id, data=fishfat2)
anova(model4)


# Get the variance components
vc <- VarCorr(model4)
vc

# Get the marginal means
popMeans(model4, effect="Species")

# Multiple comparison 
library(multcomp)

model4.tukey <- glht(model4, linfct = mcp(Species = "Tukey"))
summary(model4.tukey)
confint(model4.tukey)

model4.tukey.cld <- cld(model4.tukey)  # joined line plot
model4.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model4.tukey)
plot(model4.tukey.cld, main="Multiple comparison results", 
         xlab="Species",
         ylab="Fat")
par <- old.par


#Check the residuals etc
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(model4, resid(., type = "p") ~ fitted(.) | Species, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(model4, Species ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(model4, Fat ~ fitted(.) | Species, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(model4, ~ resid(., type = "p") | Species, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))



