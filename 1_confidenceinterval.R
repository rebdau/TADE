#univariate sample statistics
#In the "ufc_processed" dataset, what are the mean, median, standard deviation, minimum, maximum, and interquartile range of the diameter (in centimeter)?
ufc <- read.csv("../data/ufc_processed.csv")
summary(ufc)
mean(ufc$dbh.cm, na.rm = TRUE)
median(ufc$dbh.cm, na.rm = TRUE)
sd(ufc$dbh.cm, na.rm = TRUE)
range(ufc$dbh.cm, na.rm = TRUE)
IQR(ufc$dbh.cm, na.rm = TRUE)
quantile(ufc$dbh.cm, probs=c(0,0.25, 0.75),na.rm = TRUE)
#and the standard deviation by species?
tapply(ufc$dbh.cm, ufc$species, sd, na.rm = TRUE)
#what is the variance of the height (in meter) ?
var(ufc$height.m[!is.na(ufc$height.m)]) #is the same as  var(ufc$height.m, na.rm=T)
#what is the covariance between height (m) and diameter (cm)?
var(ufc$height.m[!is.na(ufc$height.m)],ufc$dbh.cm[!is.na(ufc$height.m)])
#what is the correlation between height (m) and diameter (cm)?
cor(ufc$height.m[!is.na(ufc$height.m)],ufc$dbh.cm[!is.na(ufc$height.m)])



##Problem 1
#Assume the population standard deviation s of the student height in survey is 9.48. 
#Find the best point estimate of the population mean and 
#the margin of error and 
#interval estimate at 95% confidence level.

##Solution
library(MASS)                  # load the MASS package 
height.response <- na.omit(survey$Height)
#The best point estimate of the population mean is the sample mean
samplemean <- mean(height.response)		#point estimate of the population mean
#since sd (sigma) is known, we can use the z-distribution
#Since there are two tails of the z-distribution, 
#the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail. 
#Therefore, za/2 is given by qnorm(0.975). 
#The margin of error is za/2 multiplied by standard error of the mean
sigma <- 9.48
n <- length(height.response)
sem <- sigma/n^0.5
E <- qnorm(0.975)*sem       # margin of error 
#We then add it up with the sample mean, and find the confidence interval as told.
samplemean + c(-E, E) 

##Answer
#Assuming the population standard deviation s being 9.48, 
#the margin of error for the student height survey at 95% confidence level is 1.2852 centimeters. 
#The confidence interval is between 171.10 and 173.67 centimeters.

##Alternative solution
#Instead of using the textbook formula, we can apply the z.test function in the TeachingDemos package. 
#It is not a core R package, and must be installed and loaded into the workspace beforehand.
library(TeachingDemos)         # load TeachingDemos package 
z.test(height.response, sd=sigma) #compute confidence interval on the mean of a population when the standard deviation of the population is known

##Problem 2: the real world
#Without assuming the population standard deviation of the student height in survey, 
#find the margin of error and interval estimate at 95% confidence level.

##Solution
#The best point estimate of the population mean is the sample mean
samplemean <- mean(height.response)		#point estimate of the population mean
#sd (sigma) is not known, so s must replace it
s <- sd(height.response)
#if the sample size is bigger than 30 we can still use the z-test
n <- length(height.response)
n
#with this sample size we could actually use the z-values
#But some staticians have a different point of view!:
#they say that z-values should be used whenever sigma is known and t-values whenever sigma is unknown
#Therefore, we will use here the t-distribution
#Since there are two tails of the t-distribution, 
#the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail. 
#Therefore, ta/2 is given by qt(0.975, df=n-1). 
#The margin of error is ta/2 multiplied by standard error of the mean
sem <- s/n^0.5	#standard error (of the mean)
E <- qt(0.975, df=n-1)*sem       # margin of error 
E
#We then add it up with the sample mean, and find the confidence interval as told.
samplemean + c(-E, E) 

##Answer
#Without assumption on the population standard deviation, 
#the margin of error for the student height survey at 95% confidence level is 1.3429 centimeters. 
#The confidence interval is between 171.04 and 173.72 centimeters.

##Alternative Solution
#Instead of using the textbook formula, we can apply the t.test function in the built-in statspackage.
(tt <- t.test(height.response))
#if we want to have access to the values in the result of the t.test, 
#we need to explore the architecture of the tt object:
names(tt)
tt$conf.int
tt$conf.int[1]
tt$conf.int[2]

#Exercice 3: find the confidence intervals for the mean for all numeric values in the ufc dataset
#we create a function to print the limits of the confidence interval
CI <- function (data) { #it's going to be a function in which the only parameter to  specify will be "data"
tt <- t.test(data)
tt$conf.int[1:2]
 }
#we can test the function on one variable
CI(ufc$height)	#does it work? ok!
#we apply it to all numeric variable
#how can I find automatically the numeric variables?
sapply(ufc,class)
numericvariables <- sapply(ufc,class)=="numeric"	#I make a logical vector indicating the numeric variables
#or if I wanted to include also the integers:
#numericvariables <- sapply(ufc,class)=="numeric"|sapply(ufc,class)=="integer"
#on these numeric variables I apply my function:
sapply(ufc[,numericvariables],CI)



