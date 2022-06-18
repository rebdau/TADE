  
$ Monitoring fry density over time.

# Salmon fry densities were monitored at 4 different rivers. Up to three 
# sites were selected from each river. Fry densities were measured in a 
# number of years. Not all sites were monitored in all years.

# Lines starting in ##--part001b; and ##---part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.


# Read in the data
fry.wide <- read.csv("fry.csv", header=TRUE, na.strings=".")
fry.wide
# We need to stack the data
fry <- reshape(fry.wide, varying=c('X2000','X2001','X2002','X2003','X2004'),
       v.names='Density',
       times=2000:2004, timevar='Year',
       direction="long")
# Drop missing values
fry <- fry[!is.na(fry[,"Density"]),]
fry <- cbind(fry,logDensity=log(fry[,"Density"]))
fry[1:10,]

# Get a preliminary plot of the data
plot(fry[,"Year"],fry[,"Density"],
   main='Fry density over time', xlab='Year',ylab='Fry Density')
temp <- lapply(split(fry,list(Location=fry[,"Location"],Site=fry[,"Site"])),
    function(x){
    	lines(x$Year,x$Density)})

# Get a preliminary plot of the data
plot(fry[,"Year"],fry[,"logDensity"],
   main='log(Fry density) over time', xlab='Year',ylab='log(Fry Density)')
temp <- lapply(split(fry,list(Location=fry[,"Location"],Site=fry[,"Site"])),
    function(x){
    	lines(x$Year,x$logDensity)})



#--------------------------------------------------
# Method 1. Compute the averages 

# Find the average of the fry density for each Site Location
# Use the summaryBy() function in the doBy package

library(doBy)
avg <- summaryBy(logDensity ~ Location + Year , FUN=mean, data=fry)
avg
# Plot the data
boxplot(logDensity.mean ~ Year, data=avg,  main="Avg log(density) in each year", 
        sub='Whiskers extend to range of data')
stripchart(logDensity.mean ~ Year, data=avg, add=TRUE, 
           vertical=TRUE, method="jitter", jitter=.1,  
           main="Avg log(density) for each year", 
           sub='Whiskers extend to range of data',
           xlab='', ylab='Average log(Density)')

# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(logDensity.mean ~ Year, data=avg, FUN=c(length,mean,sd))
# SE not computed in the usual way because this is not a CRD
report




# fit the linear model and get the ANOVA table and test for effects
avg$Location <- as.factor(avg$Location)
avg$Year <- as.factor(avg$Year)

# Year is usually specified last in model because R gives Type I tests.
# In this case, the design is balanced so it doesn't matter.
model <- lm(logDensity.mean ~ Location + Year, data=avg)
anova(model)




# Estimate the marginal means.
# Use the popMeans() function in the doBy package

lsmeans <- popMeans(model, eff="Year")
lsmeans


# Now for a multiple comparison procedures

# The multiple comparison package has lots of good routines.
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model.tukey <- glht(model, linfct = mcp(Year = "Tukey"))
summary(model.tukey)
confint(model.tukey)

model.tukey.cld <- cld(model.tukey)  # joined line plot 
model.tukey.cld

old.par <- par() 
layout(matrix(1:2,2,1))
plot(model.tukey)
plot(model.tukey.cld, main="Multiple comparison results", 
       xlab="Year",
       ylab="fat", 
       notch=FALSE)
par <- old.par


# Check the assumptions of the linear model on the averages
layout(matrix(1:4,2,2))
plot(model)



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation


# Plot the data
boxplot(logDensity ~ Year, data=fry,  main="logDensity for each year-location-site", 
       sub='Whiskers extend to range of data')
stripchart(logDensity ~ Year, data=fry, add=TRUE, 
       vertical=TRUE, method="jitter", jitter=.1,  
       main="logDensity over time", 
       sub='Whiskers extend to range of data',
       xlab='', ylab='logDensity')



# Fit the random effects model
# Be sure to specify that Year and Location are factors
fry$Year <- as.factor(fry$Year)
fry$Location <- as.factor(fry$Location)


library(nlme)
# Create a unique Site-Location id for each combination of Site and Location

fry$Site.id <- interaction(fry$Location,fry$Site)
model2 <- lme( logDensity ~ Location + Year, random=~1 | Site.id, data=fry)
anova(model2)

# There is NO easy way in R to specify that Year*Location is a random effect (the flume)
# and is a random effect. You would hope that 
model2b <- lme( logDensity ~ Location + Year, random=~ 1 | Location/Site , data=fry)
# The following ANOVA table is WRONG
anova(model2b)
# would work, but R gets upset and gives nonsense results in the ANOVA 
# table. Bummer. 


# Get the variance components
vc <- VarCorr(model2)
vc


# Get the marginal means
#Note that the marginal means cannot be computed in R, and I don't know how to fix this
popMeans(model2, effect='Year')

# Multiple comparison 
library(multcomp)

model2.tukey <- glht(model2, linfct = mcp(Year = "Tukey"))
summary(model2.tukey)
#ote that the confidence limits below are WRONG, and I don't know how to fix them
confint(model2.tukey)

model2.tukey.cld <- cld(model2.tukey)  # joined line plot
model2.tukey.cld


old.par <- par() 
layout(matrix(1:2,2,1))
plot(model2.tukey)
#Note that the Joined Letter plot isn't produced, and I don't know how to fix them

plot(model2.tukey.cld, main="Multiple comparison results", 
         xlab="Year",
         ylab="logDensity", 
         notch=FALSE)
par <- old.par



#Check the residuals etc
# lme() uses Trellis graphics, so the usual plotting commands are not useful
plot1 <- plot(model2, resid(., type = "p") ~ fitted(.) | Year, abline = 0)
# box-plots of residuals by Subject
plot2 <- plot(model2, Year ~ resid(.))
# observed versus fitted values by Subject
plot3 <- plot(model2, logDensity ~ fitted(.) | Year, abline = c(0,1))
# normak probabability plot of the residuals.
plot4 <- qqnorm(model2, ~ resid(., type = "p") | Year, abline = c(0, 1))

print(plot1, split=c(1,1,2,2), more=TRUE)
print(plot2, split=c(1,2,2,2), more=TRUE)
print(plot3, split=c(2,1,2,2), more=TRUE)
print(plot4, split=c(2,2,2,2))



# Do the power analysis

group.means <- c(0, 0.22)
power <- power.anova.test(groups=length(group.means), 
         within.var=0.62**2,  # note that the variance is needed
         between.var=var(group.means), 
         power=.80,
         sig.level=0.05)
power

