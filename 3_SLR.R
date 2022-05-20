##Simple Linear Regression Example
##create regdata
regdata <- data.frame(Observation = 1:18,Temperature=c(rep(0,3),rep(15,3),rep(30,3),rep(45,3),rep(60,3),rep(75,3)), weight=c(8,6,8,12,10,14,25,21,24,31,33,28,44,39,42,48,51,44))
write.csv(regdata, row.names=FALSE, file="../data/regdata.csv")
##or load
regdata <- read.csv("../data/regdata.csv")
obs <- regdata$Observation
weight <- regdata$weight
temp <- regdata$Temperature
tempsq <- temp^2
tempcub <- temp^3
logtemp <- log(temp) #natural logarithm: basd on e
##plot the weight vs temp and vs transformed temp - which appears more linear?
opar <- par()
par(mfrow=c(2,2))
plot(weight~temp)
plot(weight~tempsq)
plot(weight~tempcub)
plot(weight~logtemp)
par <- opar
##estimate slope and intercept 
xdev <- temp-mean(temp)
ydev <- weight-mean(weight)
xdev.sq <- xdev^2
ydev.sq <- ydev^2
ssx <- sum(xdev.sq)
ssy <- sum(ydev.sq)
spxy <- sum(xdev*ydev)
b1 <- spxy/ssx
b0 <- mean(weight)-(b1*mean(temp))
c(b1,b0)
##calculate residuals for the equation and the sum of squared error (SSE)
ypred <- b0 + b1*temp
residual <- weight - ypred
residualsq <- residual^2
sse <- sum(residualsq)
##test significance of regression using the ANOVA approach
#create anova table
n <- length(temp)
ssr <- ssy-sse
source <- c("regression", "residual", "total")
df <- c(1, n-2, n-1)
ss <- c(ssr, sse, ssy)
ms <- ss/df
anovatable <- data.frame(source, df, ss, ms)
F <- anovatable$ms[1]/anovatable$ms[2]
#What is the chance of having this F-statistic or more extreme (this means thus higher) if the H0 (beta1 = 0) holds true?
pvalue <- pf(F,df1=1, df2=n-2, lower.tail=FALSE)
#Is this a good equation based on goodness of fit measures?
r2 <- ss[1]/ss[3]
see <- sqrt(ms[2])
c(r2,see)
##Are the assumptions met? 
##residuals plot
plot(residual~ypred, xlab = "predicted weight", ylab="residuals")
abline(0,0)
boxplot(residual)
hist(residual)
##normality plot
sorted.resids <- residual[order(residual)]
stand.resids <- sorted.resids/(sqrt(ms[2]))  #semistudentized residuals
quantile.number <- (1:18)/18 #probability to get that value or lower in my sample set
prob.z.dist <- pnorm(stand.resids)  #probability to get that value or lower if it's part of a standard normal distribution N(0,1)
normalquantiles <- qnorm(quantile.number) #normal values corresponding to these probabilities

par(mfrow=c(2,2))
plot(prob.z.dist~quantile.number, xlab="probability in sample", ylab="probability norm")
plot(sorted.resids~normalquantiles, xlab="norm quantiles", ylab="sorted residuals")
qqnorm(residual)
library(car)
qqPlot(residual) #compare with the built-in function of the {car} package
par <- opar

##calculate 95% confidence intervals for b0 and b1
#t(alpha,n-2) with two tails
alpha <- 0.05
t95 <- qt(1-(alpha/2),n-2)
#estimated standard error of b1
mse <- ms[2]
sb1 <- sqrt(mse/ssx)
#sb0
sb0 <- sqrt(mse*((1/n)+((mean(temp))^2/ssx)))
#confidenceintervals
CIb1 <- c(min= (b1-(t95*sb1)), max = b1 + (t95*sb1))
CIb0 <- c(min= b0-(t95*sb0), max = b0 + (t95*sb0))
cbind(CIb1,CIb0)

##could the true intercept be 0?
#H0: b1=0; Ha: b1 is not 0
#if H0 is true, then b1/sb1 is distributed as t(1-alpha/2, n-2)
abs(b1/sb1)<t95 #if TRUE, we accept H0, if FALSE, we reject H0
#what is the chance to have this value for b1/sb1 if H0 is true?
pt(b1/sb1, n-2, lower.tail=FALSE)

#given a temperature of 22, what is the estimated average weight (predicted value) 
#and a 95% confidence interval for this estimate?
predictedaverage <- b0+(b1*22)
stdev <- sqrt(mse*((1/n)+((22 - mean(temp))^2/ssx)))
CI <- c(min= (predictedaverage-(t95*stdev)), max = predictedaverage+ (t95*stdev))

#given a temperature of 22, what is the estimated weight for any new observation 
#and a 95% confidence interval for this estimate?
stdev.any <- sqrt(mse*(1+(1/n)+((22 - mean(temp))^2/ssx)))
CI <- c(min= (predictedaverage-(t95*stdev.any)), max = predictedaverage+ (t95*stdev.any))

###Dedicated R-functions
##Fitting the model
model <- lm(weight~temp)
model
summary(model)
names(model)
model$coefficients
model$residuals
coefficients(model)
##Are the assumptions met?
library(car)
qqPlot(model)
qqPlot(residuals(model), id.method = "identify")
shapiro.test(residuals(model))
plot(fitted(model), rstudent(model), ylim=c(-2.5,2.5), xlim=c(0,55))
abline(h=0, lty=2)
abline(h=c(-2,2), lty=2, col="red")
identify(rstudent(model)~fitted(model))
influencePlot(model, id.method="identify")# areas of the circles proportional to Cook's distances
plot(fitted(model), cooks.distance(model))
cutoff <- 1
abline(h=cutoff, lty=2, col="red")
identify(fitted(model), cooks.distance(model))
opar <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), las = 1)
plot(model)
par(opar)
remove = -c(5, 18)
remove
model.rebuild <- lm(model, subset=remove)
##Is the regression significant?
confint(model, level=0.95)
anova(model)
predict(model)
temp.new <- data.frame(temp = c(22, 32, 64))
temp.new
y.hat.new <- predict(model, newdata=temp.new)
y.hat.new
plot(temp, weight, ylim=range(y.hat.new))
points(temp.new$temp, y.hat.new, col="red", lwd=2)
abline(model, col="green")
y.hat.new.PI <- predict(model, newdata=temp.new, interval="confidence", level=0.95)
y.hat.new.PI
y.hat.new.PI <- predict(model, newdata=temp.new, interval="prediction", level=0.95)
y.hat.new.PI
plot(temp, weight, ylim=range(y.hat.new.PI))
points(temp.new$temp, y.hat.new.PI[,1], col="red", lwd=2)
abline(model, col="green")
lines(temp.new$temp, y.hat.new.PI[,2], col="red", lty=2)
lines(temp.new$temp, y.hat.new.PI[,3], col="red", lty=2)

##testing a linear model
input <- rnorm(200, mean=50, sd=12)
response <- 0.7*input + 50 + rnorm(200, sd=10)
# Create index vectors that indicate observations for building and testing:
build.index = seq(1, 150)
test.index  = seq(151, 200)
# Build the model:
model <- lm(response ~ input, subset=build.index)
summary(model)
# Test model. Create data frame from the rest of the "input" x-variable.
x.new <- data.frame(input = input[test.index])
y.hat.new.PI <- predict(model, newdata=x.new, interval="prediction", level=0.95)
# Get the actual y-values from the testing data
y.actual = response[test.index]
errors <- y.hat.new.PI[,1] - y.actual
# Calculate RMSEP, and compare to model's standard error, and residuals.
RMSEP <- sqrt(mean(errors^2))
RMSEP
summary(residuals(model))
##do the reference values lie in the 95% CI of predictions from new values?
plot(input, response, ylim=range(y.hat.new.PI))
abline(model, col="green")
lines(x.new$input, y.hat.new.PI[,2], col="red", lty=2)
lines(x.new$input, y.hat.new.PI[,3], col="red", lty=2)
points(x.new$input, y.actual, col="red", lwd=2)
