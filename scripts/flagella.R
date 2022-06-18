# Single Factor CRD with sub-sampling

# Microphotographs of algae are taken, and the 
# length of one or two flagellum of each cell is measured.

# Read in the data
lengths <- read.csv("flagella.csv", header=TRUE)
lengths$Variant.Cell <- paste(lengths$Variant,".",lengths$Cell...,sep="")
lengths[1:10,]

#--------------------------------------------------
# Method 1. Compute the average-of-the-averages

# Find the average of the lengths for each cell
# We need to stack the data, and then aggregate
stack.length <- reshape(lengths, idvar="Variant.Cell",
            varying=list(c("Flagellar.1","Flagellar.2")), 
            times=c("Flagellar.1","Flagellar.2"), timevar="Length",
            v.name="Length", 
            direction="long")
stack.length[1:10,]


avg <- aggregate(Length ~ Variant+ Cell..., FUN=mean, data=stack.length)
avg[1:10,]

# Plot the data
boxplot(Length ~ Variant, data=avg,  main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data')
stripchart(Length ~ Variant, data=avg, add=TRUE, 
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')

# Get rid of outliers
avg <- avg[avg$Length > 4.5,] 
boxplot(Length ~ Variant, data=avg,  main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data')
stripchart(Length ~ Variant, data=avg, add=TRUE, 
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Avg flagella length for each cell", 
   sub='Whiskers extend to range of data',
   xlab='', ylab='Average length')

# Compute some summary statistics for each group
library(doBy)
report <- summaryBy(Length ~ Variant, data=avg, FUN=c(length,mean,sd))
report$Length.se <- report$Length.sd/sqrt(report$Length.length)
report

# fit the linear model and get the ANOVA table and test for effects
model <- lm(Length ~ Variant, data=avg)
anova(model)


# Check the assumptions of the ANOVA model
layout(matrix(1:4,2,2))
plot(model)

# Estimate the marginal means.
library(doBy)
lsmeans <- popMeans(model, eff="Variant")


# Now for a multiple comparison procedures



# The multiple comparison package has lots of good routines.
# specify all pair-wise comparisons among levels of variable "tension"
# This is a bit of R magic -- see the help pages and examples of the packages
# for details
library(multcomp)

model.tukey <- glht(model, linfct = mcp(Variant = "Tukey"))
model.tukey.cld <- cld(model.tukey)  # joined line plot

# create the display 
model.tukey.cld
old.par <- par( mai=c(1,1,1.25,1)) # set top margin bigger
plot(model.tukey.cld, # main="Multiple comparison results", 
     xlab="Variant",
     ylab="Length", 
     notch=TRUE)



#---------------------------------------------------------------
# Method 2. Fit a model that accounts for the two sources of variation

#import data with outlier deleted
lengths <- read.csv("flagella2.csv", header=TRUE)

# We start by stacking the data
# Refer to http://gbi.agrsci.dk/~shd/misc/Rdocs/reshape.pdf for details
# on the reshape() function

stack.length <- reshape(lengths, idvar="Variant.Cell",
            varying=list(c("Flagellar.1","Flagellar.2")), 
            times=c("Flagellar.1","Flagellar.2"), timevar="Length",
            v.name="Length", 
            direction="long")
stack.length[1:10,]
 # get rid of outliers
stack.length <- stack.length[!(stack.length$Length<10 & 
                             stack.length$Variant=="A"),]
stripchart(Length ~ Variant, data=stack.length)


# Fit the random effects model
library(lme4)
model2 <- lmer( Length ~ Variant + (1|Variant.Cell), data=stack.length)
summary(model2)
anova(model2) # note no p-value given

# Get the marginal means
# There is no easy wasy to do this using lmer()
pred.Variant <- expand.grid( Variant=unique(avg$Variant), Length=0)
mm <- model.matrix(terms(model2),pred.Variant)
means <- mm %*% fixef(model2)
means.vcv <- mm %*% tcrossprod(vcov(model2),mm)
means.se <- sqrt(diag(mm %*% tcrossprod(vcov(model2),mm)))
cbind(pred.Variant, means, means.se)

# Multiple comparison 
library(multcomp)

model2.tukey <- glht(model2, linfct = mcp(Variant = "Tukey"))
model2.tukey.cld <- cld(model2.tukey)  # joined line plot

# create the display 
model2.tukey.cld
old.par <- par( mai=c(1,1,1.25,1)) # set top margin bigger
plot(model2.tukey.cld, # main="Multiple comparison results", 
     xlab="Variant",
     ylab="Length", 
     notch=TRUE)






