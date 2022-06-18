setwd(choose.dir())

#setwd("C:/Users/Rebecca/Documents/Cours/201516/TADE/coursTADE2016/workdirectory")


# Tuscaloosa Average Yearly Temperatures

# Consider a time series of annual average temperatures measured at 
# Tuscaloosa, Alabama from 1901 to 2001. 
# It is well known that shifts in temperature can occur whenever the 
# instrument or location or observer or other characteristics of the station change.

# In this case, this corresponds to the years
# 1901-1938 (inclusive); 1940-1956 (inclusive); 1958-1986 (inclusive), and 
# 1989-2000 (inclusive). Note that the years 1939, 1957, and 1987 are NOT 
# used because the average #temperature in these two years is an 
# amalgam of two different recording #conditions


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


# Read in the data. Declare Epoch as a factor. Remove data points when location changed in a year

tusctemp <- read.csv("../data/tuscaloosa.csv", header=TRUE, 
                  as.is=TRUE, strip.white=TRUE,
                  na.string="")  # here missing values are blanks or null cells
tusctemp <- tusctemp[complete.cases(tusctemp[,c("Year","Epoch","Avg.Temp..C.")]),] # drop the missing values
tusctemp$Epoch <- factor(tusctemp$Epoch)
head(tusctemp)
str(tusctemp)

# make an initial plot of the data
# Notice how we specify a different plotting symbol for each Epoch.
plotprelim <- ggplot(data=tusctemp, aes(x=Year, y=Avg.Temp..C., 
                      shape=Epoch, color=Epoch))+
  ggtitle("Tuscaloosa Avg Temp (C) over time")+
  xlab("Year")+ylab("Tuscaloosa Avg Temp (C)")+
  geom_point(size=4)
plotprelim


# Look at the data around an epoch change
tusctemp[ tusctemp$Year %in% 1936:1945,]


# Fit a separate line for each epoch and plot them
d_ply(tusctemp, "Epoch", function(x){
  # fit a separate line for each Epoch
  cat("\n\n***Separate fit for Epoch :", as.character(x$Epoch[1]),"\n")
  model <- lm( Avg.Temp..C. ~ Year, data=x)
  print(summary(model))
  print(confint(model)) # confidence interval on slope
})

prelimplot2 <- plotprelim +
  geom_smooth(method="lm", se=FALSE)
prelimplot2

# Fit the regression line with non-parallel slopes and look at the ANOVA table
# Because lm() produces type I (increment tests), you need to specify the
# interaction term last in the model sequence.
# Be sure that Epoch has been declared as a factor.
tusctemp.model.np <- lm( Avg.Temp..C. ~ Epoch + Year + Year:Epoch, data=tusctemp)
anova(tusctemp.model.np)

tusctemp.model.np.lsmo <- lsmeans::lstrends(tusctemp.model.np, ~Epoch, var="Year")
summary(tusctemp.model.np.lsmo, infer=TRUE)
cld(tusctemp.model.np.lsmo)





# Fit the regression line with parallel slopes. Specify the Epoch term last
# to get the proper test for Epoch effects
# Be sure that Epoch has been declared as a factor.
# Notice that the anova() table term for Year is NOT useful
tusctemp.model.p <- lm( Avg.Temp..C. ~ Year + Epoch, data=tusctemp)
summary(tusctemp.model.p)

# Extract the individual parts of the fit using the 
# standard methods. Note that because Epoch is a factor
# DO NOT USE THE ESTIMATES from the summary() to estimate
# the Epoch effect because these estimates depend on the
# internal parameterization used. Use the lsmeans() function instead
summary(tusctemp.model.p)
coef(tusctemp.model.p)
sqrt(diag(vcov(tusctemp.model.p))) # gives the SE
confint(tusctemp.model.p)
names(summary(tusctemp.model.p))
summary(tusctemp.model.p)$r.squared
summary(tusctemp.model.p)$sigma

# check for autocorrelation using Durbin-Watson test.
# You can use the durbinWatsontest in the car package or the
#                 dwtest in the lmtest package
# For small sample sizes both are fine; for larger sample sizes use the lmtest package
# Note the difference in the default direction of the alternative hypothesis

  durbinWatsonTest(tusctemp.fit.p) # from the car package
  dwtest(tusctemp.fit.p) # from the lmtest package


# Estimate the size of the Epoch effects. Do not use
# the output from summary() directly as this depends on the
# internal parameterization used by R. We use the lsmeans() package
tusctemp.model.p.lsmo <- lsmeans::lsmeans(tusctemp.model.p, ~Epoch)
Epochdiff <- pairs(tusctemp.model.p.lsmo)
summary(Epochdiff, infer=TRUE)
cld(tusctemp.fit.p.lsmo)

# plot the fitted lines to the graphs
# Because there are multiple lines, it is better to make predictions and
# then plot the predictions on the plot. See later.

# make predictions
# First set up the points where you want predictions
newYears <- rbind( tusctemp[,c("Year","Epoch")],
                   data.frame(Year=2001:2030, Epoch=tusctemp$Epoch[tusctemp$Year==2000]))
newYears[1:5,]
str(newYears)


# Predict the AVERAGE temperature in each Year
# You need to specify help(predict.lm) tp see the documentation
predict.avg <- predict(tusctemp.model.p, newdata=newYears, 
                       se.fit=TRUE,interval="confidence")
# This creates a list that you need to restructure to make it look nice
predict.avg.df <- cbind(newYears, predict.avg$fit, se=predict.avg$se.fit)
head(predict.avg.df)
temp <- predict.avg.df[predict.avg.df$Year==2020,]
temp

plotfit <- plotprelim + 
     geom_line(data=predict.avg.df, aes(x=Year, y=fit))
plotfit
# Add the confidence intervals to the plot
plotfit.avgci <- plotfit +
    geom_ribbon(data=predict.avg.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.2)
plotfit.avgci

# Predict the INDIVIDUAL average temperature. This again needs
# to be interpretted carefully. This gives the potential range
# in the yearly average in a selected year and not the
# range in the individual daily temperatures.
# R does not product the se for individual predictions
predict.indiv <- predict(tusctemp.model.p, newdata=newYears, 
                        interval="prediction")
# This creates a list that you need to restructure to make it look nice
predict.indiv.df <- cbind(newYears, predict.indiv)
head(predict.indiv.df)
predict.indiv.df    [predict.indiv.df$Year==2020,]
temp <- predict.avg.df[predict.avg.df$Year==2020,]
temp


# Add the prediction intervals to the plot
plotfit.indivci <- plotfit.avgci +
    geom_ribbon(data=predict.indiv.df, aes(x=Year,y=NULL, ymin=lwr, ymax=upr),alpha=0.1)
plotfit.indivci




# No easy way to do inverse predictions
# except perhaps plot the two confidence curves and the draw a line
# to see where it crosses the confidence curves that need to be extended

plotinvpred <- plotfit.indivci +
  geom_hline(yintercept=19)
plotinvpred


# look at diagnostic plot
# There is some evidence of non-fit in the qq pot and the scale-location plot.
plotdiag <- autoplot(tusctemp.model.p)
plotdiag  
plotdiag <- sf.autoplot.lm(tusctemp.fit.p)




###################################################################################
# Kendall test for trend not really sensible here because of the regime shift
# as the epoch changes.
