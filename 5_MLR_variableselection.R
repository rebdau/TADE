##############################################################################################
# Variable selection
##############################################################################################


standdat<- read.table("../data/stand.txt",header=TRUE)

attach(standdat)
names(standdat)
standdat


#############################################################################################
#  Stepwise regression
#  Using step to do stepwise methods to help in choosing x variables.  Starts with all
#  x variables, as shown by volha ~ . and the specific data frame. First one uses
#  forward (in only), second one uses backward (out only), and last allows
#  variables to be dropped and later to enter back in (both).  
############################################################################################
model1 <- lm(vol.ha ~ .,data = standdat)
summary(model1)
step.model1 <- step(model1,direction=c("forward"))
summary(step.model1)

step.model2 <- step(model1,direction=c("backward"))
summary(step.model2)

step.model3 <- step(model1,direction=c("both"))
summary(step.model3)

#############################################################################################
#  All Subsets Regression
############################################################################################

library(leaps)
leaps<-regsubsets(vol.ha ~age+si+ba.ha+stems.ha+topht+qdbh,data=standdat,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
par(mfrow=c(1,2),cex=0.7)
plot(leaps,scale="r2")
plot(leaps,scale="adjr2")
par(mfrow=c(1,1),cex=1)

# plot statistic by subset size 
library(car)
par(mfrow=c(1,2))
subsets(leaps, statistic="rsq")
subsets(leaps, statistic="adjr2")
par(mfrow=c(1,1))

