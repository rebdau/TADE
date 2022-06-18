setwd(choose.dir())

#setwd("C:/Users/Rebecca/Documents/Cours/201516/TADE/coursTADE2016/workdirectory")



# Degradation of dioxin in crabs - II

# How fast does dioxin degrade over time? In each Year, samples of crabs
# were captured at a site. The crab livers were excised, composited #together, and various species of dioxings were measured. These were #translated into
# the World Health Organization (WHO) standardized total equivalent dose.

# This was done for two different sites. Is the degradation rate the same
# at both sites?


library(car)
#library(ggfortify)
library(ggplot2)
library(Kendall)
library(lmtest)
library(lsmeans)
library(plyr)


#install first the packages tibble and tidyr!
install.packages("../scripts/ggfortify_0.2.0.tar.gz", dependencies=TRUE, repos = NULL, type="source")

library(ggfortify)

#source("../scripts/helperfunctions.r")




# Read in the data. Declare Site as a factor
crabs <- read.csv("../data/dioxin2.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string=".")
crabs$Site   <- factor(crabs$Site)
crabs$logTEQ <- NULL # drop this and recompute later
head(crabs)
str(crabs)

# make an initial plot of the data
# Notice how we specify a different plotting symbol for each site.
ggplot(data=crabs, aes(x=Year, y=WHO.TEQ, 
                      shape=Site, color=Site))+
  ggtitle("Dioxin levels over time")+
  xlab("Year")+ylab("Dioxin levels (WHO.TEQ)")+
  geom_point(size=4)



# Find the logTEQ and then get a revised plot

crabs$logTEQ <- log(crabs$WHO.TEQ)
head(crabs)

# Repeat the plots on the log-scale
plotprelimlog <- ggplot(data=crabs, aes(x=Year, y=logTEQ, shape=Site, color=Site))+
  ggtitle("log(Dioxin) levels over time")+
  xlab("Year")+ylab("log(Dioxin) levels (WHO.TEQ)")+
  geom_point(size=4)
plotprelimlog

# Fit a separate line for each year
d_ply(crabs, "Site", function(x){
  # fit a separate line for each site
  cat("\n\n***Separate fit for site :", as.character(x$Site[1]),"\n")
  model <- lm( logTEQ ~ Year, data=x)
  print(summary(model))
  print(confint(model)) # confidence interval on slope
})


# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that Site has been declared as a factor.
crabs.model.np <- lm( logTEQ ~ Site + Year + Year:Site, data=crabs)
anova(crabs.model.np)

# estimate the individual slopes and compare them 
crabs.model.np.lsmo <- lsmeans::lstrends(crabs.model.np, ~Site, var="Year")
summary(crabs.model.np.lsmo, infer=TRUE)
summary(pairs(crabs.model.np.lsmo), infer=TRUE)

# Fit the regression line with parallel slopes. Specify the Site term last
# to get the proper test for site effects
# Be sure that Site has been declared as a factor.
crabs.model.p <- lm( logTEQ ~ Year + Site, data=crabs)
anova(crabs.model.p)


# Extract the individual parts of the fit using the
# standard methods. Note that because Site is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the site effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(crabs.model.p)
coef(crabs.model.p)
sqrt(diag(vcov(crabs.model.p))) # gives the SE
confint(crabs.model.p)
names(summary(crabs.model.p))
summary(crabs.model.p)$r.squared
summary(crabs.model.p)$sigma

# check for autocorrelation using Durbin-Watson test.
# You will need to do this for EACH site as the two sites are measured
# in the same year.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

d_ply(crabs, "Site", function(x){
  # apply the DW test to each sites regression lines
  cat("***** DW test for site :", as.character(x$Site[1]),"\n")
  crabs.model <- lm(logTEQ ~ Year, data=x)
  print(durbinWatsonTest(crabs.model )) # from the car package
  print(dwtest(crabs.model )) # from the lmtest package
})

# Estimate the size of the site effect. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
crabs.model.p.lsmo <- lsmeans::lsmeans(crabs.model.p, ~Site)
sitediff <- pairs(crabs.model.p.lsmo)
summary(sitediff, infer=TRUE)

# plot the fitted lines to the graphs
# Because there are two lines, it is better to make predictions and
# then plot the predictions on the plot. See later.



# make predictions
# First set up the points where you want predictions
newYears <- expand.grid(Year=seq(min(crabs$Year,na.rm=TRUE),2030,1), Site=unique(crabs$Site))
newYears[1:5,]
str(newYears)

# Predict the AVERAGE duration of cuting at each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(crabs.model.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)
temp <- predict.avg.df[predict.avg.df$Year==2010,]
temp
cat("Estimates of average amount of dioxin on the anti-log scale \n")
cbind(temp[,c("Year","Site")], exp(temp[,c("fit","lwr","upr")]))

plotfit <- plotprelimlog + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit))
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)+
    xlim(c(1990,2010))
plotfit.avgci

########################################################################
#######################################################################
# You can plot everything back on the anti-logscale as well - try it!


# Predict the INDIVIDUAL duration of cuting at each Year
# R does not product the se for individual predictions
predict.indiv <- predict(crabs.fit.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
predict.indiv.df    [predict.indiv.df$Year==2010,]
temp <- predict.avg.df[predict.avg.df$Year==2010,]
temp
cat("Estimates of individual levels of dioxin on the anti-log scale \n")
cbind(temp[,c("Year","Site")], exp(temp[,c("fit","lwr","upr")]))


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci


# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- autoplot(crabs.fit.p)
plotdiag  # 2016-01-17 bug in ggfortify doesn't let you save using ggsave
plotdiag <- sf.autoplot.lm(crabs.fit.p)


# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended

plotinvpred <- plotfit.indivci +
  geom_hline(yintercept=log(10))+
  xlim(c(1990, 2030))
plotinvpred



###################################################################################
# Kendall test for trend. Need to do this for EACH site.
# You cannot test for parallelism of trends in sites using Kendall's tau
# Need to order the data by time (in advance of the call)
# This assumes that autocorrelation is negligible. Refer to other examples
# in this chapter when this is not the case

d_ply(crabs,"Site", function(x){
  # Apply kendalls method to each site
  cat("\n\n***** Kendalls tau for site: ", as.character(x$Site[1]),"\n")
  tau.test <- MannKendall(x$WHO.TEQ)
  print(summary(tau.test))
  # Notice that the test are invariant to transformations
  tau.test.log <- MannKendall(x$logTEQ)
  print(tau.test.log)
})


# Now to estimate the slope (on the log-scale) using the Sen estimator in the zyp package
library(zyp)
sen.slope <- zyp.sen(logTEQ~Year, data=crabs)
sen.slope$coef
confint.zyp(sen.slope)




































