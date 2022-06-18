# Stream residence time of salmon

# The stream residence of time was measured for individually tagged fish in 
# a number of years.

# Example of a two factor CRD analysis of variance with unbalanced data



# Read in the actual data

restime <- read.csv("residence.csv", header=TRUE)
restime$trt <- interaction(restime$Sex, restime$Year)
restime$Year <- as.factor(restime$Year)
restime[1:10,]

# Preliminary plot

# Get side-by-side dot plots
boxplot( ResidenceTime ~ Sex+Year, data=restime, range=0,
      main="restime levels in different Sex/Year of fish", 
   sub='Whiskers extend to range of data',
   xlab='Sex+Speciex', ylab='restime levels')

stripchart(ResidenceTime~ Sex+Year, data=restime, add=TRUE, 
     vertical=TRUE, method="jitter", jitter=.1)

# Get some simple summary statistics
library(plyr)
report <- ddply(restime, c("Sex","Year"),sf.simple.summary, variable="ResidenceTime", crd=TRUE)
report


# Draw a profile plot

attach(restime)
interaction.plot(Year,Sex,ResidenceTime,
ylim=c(min(report$mean-2*report$se),
         max(report$mean+2*report$se)),
  main="Profile plot with approx 95% ci")
detach(restime)

# add the approximate 95% ci to each point
segments(as.numeric(report$Year), report$mean-2*report$se, 
         as.numeric(report$Year), report$mean+2*report$se)



# Fit the linear model, get the anova table, and the usual stuff
# CAUTION!!! Because the design is unbalance, the default model
# fit by aov gives the WRONG sum of squares and F-tests.
# The default tests are "sequential tests" where terms are added
# in the order specified. You want the marginal tests 
# (which are reported in JMP or SAS)
#
# Read the entry at 
#  http://r-eco-evo.blogspot.com/2007/10/infamous-type-iii-ss-before-i-started_20.html
#
# You can also use the Anova() function from the car package.
#The sum of squares and F-tests from the anova() below are INCORRECT in unbalanced data
#because they are sequential and only adjust for effect that enter the model prior to the term in question.")
model <- lm( ResidenceTime ~ Sex + Year + Sex*Year, data=restime)
# Analysis of variance -- this is NOT CORRECT because design is unbalanced
anova(model)

#Use the Type III tests from the Anova() function from the car package
#but you need to set the treatment contrasts to sum rather than treatment BEFORE fitting the lm() model!
#See http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA-td1573657.html
library(car)
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly")) 
options()$contrasts
model2 <- lm( ResidenceTime ~ Sex + Year + Sex*Year, data=restime)
Anova(model2,type=3)
options(old.options)



# Check the residuals etc
oldpar <- par(mfrow=c(2,2))
plot(model)
par <- oldpar


# LSmeans after a lm() fit
library(doBy)
#Estimated marginal means
popMeans(model, eff="Sex")

library(doBy)
#Estimated marginal means
popMeans(model, eff="Year")

library(doBy)
#Estimated marginal means
popMeans(model, eff=c("Sex","Year"))


# Do the multiple comparisons
# The multiple comparison package has lots of good routines.
# specify all pair-wise comparisons among levels of variable "tension"
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

#Multiple comparison for Sex effect - be sure to specify interaction_average=TRUE
model.mcp.Sex     <- glht(model, linfct = mcp(Sex = "Tukey", interaction_average=TRUE))
summary(model.mcp.Sex)
confint(model.mcp.Sex)
model.mcp.Sex.cld <- cld(model.mcp.Sex)  # joined line plot
model.mcp.Sex.cld
plot(model.mcp.Sex.cld, main="Multiple comparison results", 
     xlab="Year",
     ylab="restime", 
     mai=c(1,1,1.25,1) , notch=TRUE)

##---part100multcomp-Yearb;
library(multcomp)

cat("\n\n Multiple comparison for Year effect - be sure to specify interaction_average=TRUE \n")
result.mcp.Year     <- glht(result.lm, linfct = mcp(Year = "Tukey",interaction_average=TRUE))
summary(result.mcp.Year)
confint(result.mcp.Year)
result.mcp.Year.cld <- cld(result.mcp.Year)  # joined line plot
result.mcp.Year.cld
plot(result.mcp.Year.cld, main="Multiple comparison results", 
     xlab="Year",
     ylab="restime", 
     mai=c(1,1,1.25,1) , notch=TRUE)
##---part100multcomp-Yeare;
dev.off()
sink()

sink('residence-R-100multcomp-int.txt')
png('residence-R-100multcomp-int.png')
##---part100multcomp-intb;
library(multcomp)

# There is no easy way to do the comparison on the interaction terms.
# You need to create a "pseudo-factor" that is the combination of the 
# two factors and then do the multiple comparison.
result.lm2 <- lm( ResTime ~ trt -1, data=restime)
cat("\n\n Multiple comparison for Sex*Year effect \n")
result.mcp.int     <- glht(result.lm2, linfct = mcp(trt="Tukey" ))
summary(result.mcp.int)
confint(result.mcp.int)
result.mcp.int.cld <- cld(result.mcp.int)  # joined line plot
result.mcp.int.cld
plot(result.mcp.int.cld, main="Multiple comparison results", 
     xlab="Sex.Year",
     ylab="restime", 
     mai=c(1,1,1.25,1) , notch=TRUE)
par <- oldpar
##---part100multcomp-inte;
dev.off()
sink()
