##Problem 1
#We believe that the average “normal” weight of ravens in Yukon is 1kg. 
#In a sample of 10 ravens, the average bird weight is 0.8 kg, 
#and the sample standard deviation was 0.02 kg. 
#At 0.05 significance level, 
#can we reject the null hypothesis that the mean raven weight does not differ the “normal” weight?

##Solution
samplemean <- 0.8		# sample mean 
mu0 <- 1             # hypothesized population mean
s <- 0.02                # sample standard deviation 
n <- 10                 # sample size 
sem <- s/sqrt(n)
sem
t <- (samplemean-mu0)/sem 
t                      # test statistic 
alpha <- 0.05 
t.half.alpha <- qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha) #critical values

#alternatively:
pval <- 2 * pt(t, df=n-1)  # lower tail 
pval                       # two-tailed p-value 

##Answer
#The test statistic -31.64 is not between the critical values -2.262, and 2.262. 
#Alternatively, the p-value for the test-statistic is smaller than the 0.05 significance level.
#Hence, at 0.05 significance level, 
#we reject the null hypothesis that the mean bird weight does not differ from the assumed “normal” weight of 1 kg

##Problem 2
#We assume that students are on average 20 years old. 
#Using the sample of the student population in the data set survey of the package “MASS”, 
#at 0.05 significance level, 
#can we reject the null hypothesis that the mean student age is 20 years?

##Solution
library(MASS)                  # load the MASS package 
names(survey)
age <- survey$Age
t.test(age, mu=20, alternative="two.sided")

##Answer
#the p-value for the test-statistic is higher than the 0.05 significance level.
#Hence, at 0.05 significance level, 
#we accept the null hypothesis that the mean student age is 20

##Problem 3
#We believe that the average weight of female ravens differs from average weight of male ravens. 
#A sample of 20 birds is taken and each bird is weighed and released. 
#12 birds were males with an average weight of 1.2 kg and a standard deviation of 0.02 kg. 
#8 birds were females with an average weight of 0.8 and a standard deviation of 0.01 kg.
#can we reject the null hypothesis that the male raven weight does not differ the female raven weight?

##Solution
samplemeanM <- 1.2		# male sample mean 
samplemeanF <- 0.8		# female sample mean 
samplemeandiff <- samplemeanM-samplemeanF	#difference between sample means
sM <- 0.02                # male sample standard deviation 
sF <- 0.01                # female sample standard deviation 
nM <- 12                 # male sample size 
nF <- 8                 # female sample size 
df <- nM+nF-2
sem <- sqrt((((nM-1)*(sM^2))+ ((nF-1)*(sF^2)))/df)	#standard error of mean of differences between samplemeans
t <- (samplemeandiff)/sem 
t                      # test statistic 
alpha <- 0.05 
t.half.alpha <- qt(1-alpha/2, df=df) 
c(-t.half.alpha, t.half.alpha) #critical values

#alternatively:
pval <- 2 * (1-pt(t, df=df))   
pval                       # two-tailed p-value 

##Answer
#The test statistic 23.76354 is not between the critical values -2.1, and 2.1. 
#Alternatively, the p-value for the test-statistic is smaller than the 0.05 significance level.
#Hence, at 0.05 significance level, 
#we reject the null hypothesis that the average male bird weight equals the average female bird weight.

##Problem 4
#Using the sample of the student population in the data set survey of the package “MASS”, 
#at 0.05 significance level, 
#can we reject the null hypothesis 
#that the mean height of male and female students is equal?

##Solution
library(MASS)                  # load the MASS package 
names(survey)
height <- survey$Height
is.factor(survey$Sex)
sex <- survey$Sex
t.test(height ~ sex)

##Answer
#the p-value for the test-statistic is lower than the 0.05 significance level.
#Hence, at 0.05 significance level, 
#we reject the null hypothesis that the mean male and female student heights are equal
