# Impact of development on water quality
# with ggplot,lsmeans, and lmer package

# Water quality monitoring studies often take the form of incomplete Event 
# designs. For example, the following data represents TSS in water samples 
# taken upstream of a development (the reference sample), at the 
# development (themid-stream sample), or downstream of the development 
# (the ds sample). Samples are taken during storm Events when water 
# quality may be compromised by the development. Here is a small set of data.
# Such a small set of data likely has very poor power to detect 
# anything but very large differences in water quality among  the three 
# Locations. 
#
#



library(ggplot2)
library(lmerTest)
library(lsmeans)
library(nlme)


wq <- read.csv("water-quality.csv", header=TRUE,
         na.strings = ".", as.is=TRUE)
wq$logTSS <- log(wq$TSS)
wq$Location <- as.factor(wq$Location) # convert to factors
wq$Event    <- as.factor(wq$Event)
wq
str(wq)


# Note that terms must be in the order Event + Location
# because R does incremental sums of squares and 
# not marginal SS.
model <- lm( logTSS ~ Event + Location, data=wq)
anova(model)


# Estimate the population marginal means.
# Use the lsmeans() function from the lsmeans package
model.lsmo <- lsmeans(model, ~Location)
summary(model.lsmo, infer=TRUE, adjust="tukey")


# Find all pair-wise comparisons among levels of variable "Location"
model.cld <- cld(model.lsmo)
model.cld
pairs(model.lsmo, adjust="tukey")
confint(pairs(model.lsmo, adjust="tukey"))

# Make a plot of the CLD results
plotcld <- sf.cld.plot.bar(model.cld, variable="Location")
plotcld <- plotcld + 
  xlab("Location")+
  ylab("Mean lifetime (with 95% ci")+
  ggtitle("Comparison of mean log(TSS) with cld")
plotcld



#---------------- Combined Inter and Intra-block analysis  using lmer()
#                 where blocks are declared as random effects
model.lmer <- lmer( logTSS ~Location + (1|Event), data=wq,
                   na.action=na.omit)
anova(model.lmer, ddf="Kenward-Roger")


# Estimate variance components
VarCorr(model.lmer)


# Estimate the population marginal means.
# Use the lsmeans() function from the lsmeans package
model.lmer.lsmo <- lsmeans::lsmeans(model.lmer, ~Location)
summary(model.lmer.lsmo, infer=TRUE, adjust="tukey")


# Find all pair-wise comparisons among levels of variable "Location"
model.lmer.cld <- cld(model.lmer.lsmo)
model.lmer.cld
pairs(model.lmer.lsmo, adjust='tukey')
confint(pairs(model.lmer.lsmo, adjust='tukey'))

# Make a plot of the CLD results
plotcld <- sf.cld.plot.bar(model.lmer.cld, variable="Location")
plotcld <- plotcld + 
  xlab("Location")+
  ylab("Mean lifetime (with 95% ci")+
  ggtitle("Comparison of mean log(TSS) with cld from lmer()")
plotcld


#---------------- Combined Inter and Intra-block analysis using lme()
#                 where blocks are declared as random effects
#                 This is no longer recommended because lsmeans and other
#                 packages have difficulty in finding the correct df


# We need to use lme for random effect models

model.lme <- lme( logTSS ~Location, data=wq, random=~1 | Event,
               na.action=na.omit)
anova(model.lme)


# Estimate the population marginal means.
# Notice the lsmeans() used infinite df for the comparisons

model.lme.lsmo <- lsmeans(result.lme, ~Location)
summary(model.lme.lsmo, infer=TRUE, adjust="tukey")

model.lme.cld <- cld(model.lme.lsmo)
model.lme.cld
pairs(model.lme.lsmo, adjust="tukey")
confint(pairs(model.lme.lsmo, adjust="tukey"))
