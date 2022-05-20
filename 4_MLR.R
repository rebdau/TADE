
##############################################################################################
# Multiple Linear Regression Example
##############################################################################################
rm(list=ls(all=TRUE))  # clear any remaining objects from previous R sessions

standdat<- read.table("../data/stand.txt",header=TRUE)

# since standdat was created as a dataframe, the names can be attached for simpler commands
attach(standdat)# this allows you to use the dataframe, treedat, with shorter names for variables
names(standdat)

standdat

lnvolha=log(vol.ha)
lnage=log(age)
lnbaha=log(ba.ha)

detach(standdat)# This just detaches the dataframe, standdat, but it can be reattached

standdat<-data.frame(standdat,lnvolha,lnage,lnbaha)
rm(lnvolha,lnage,lnbaha)

attach(standdat)
names(standdat)

par(mfrow=c(3,3),cex=0.7)
plot(vol.ha~age,data=standdat)
plot(vol.ha~si,data=standdat)
plot(vol.ha~ba.ha,data=standdat)
plot(vol.ha~stems.ha,data=standdat)
plot(vol.ha~topht,data=standdat)
plot(vol.ha~qdbh,data=standdat)
plot(lnvolha~lnage,data=standdat)
plot(lnvolha~lnbaha,data=standdat)
par(mfrow=c(1,1),cex=1)

##### model ######################################
model.volha<-lm(vol.ha~ba.ha+stems.ha+qdbh)  
model.volha

##I. Are the assumptions of MLR met?
yhat<-fitted(model.volha)
resid<-resid(model.volha)
cbind(vol.ha,yhat,resid) 

par(mfrow=c(2,2),cex=0.7)
plot(yhat~vol.ha)
abline(a=0,b=1)   # plot a reference line where yhat equals height 

plot(resid~yhat)  # residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1),cex=1)

shapiro.test(resid)
##II. Is the regression significant?
#-> f-test
summary(model.volha) 
names(summary(model.volha))
summary(model.volha)$fstatistic
Fvalue <- summary(model.volha)$fstatistic[1]
Fcritical <- qf(0.95,3,24)
Fvalue > Fcritical #If F-value > critical F-value, then reject H0; not all slopes are zero

anova(model.volha)#R decomposes SSR in parts that sum up to SSR
anovatable <- anova(model.volha)
ssr <- sum(anovatable[1:3,2])
ssr
dfr <- sum(anovatable[1:3,1])
dfr
msr <- ssr/dfr
mse <- anovatable[4,3]
mse
fvalue <- msr/mse
fvalue #should be the same as in summary()

##III. Regression is significant – Now: which x-variables are significant, given the other variables in the equation?
#-> t-test
summary(model.volha)
#Partial F-test
model.volha<-lm(vol.ha~ba.ha+stems.ha+qdbh)  
model.volha2<-lm(vol.ha~ba.ha+stems.ha)  
anova(model.volha2,model.volha)                # partial F test to compare the two nested models
anova(model.volha)#In R, Partial F-values for single x-variables are given in the anova table of the full model
#Sometimes we are interested in simultaneously testing whether a certain subset of the coefficients are equal to 0 (e.g. ß3 = ß4 = 0). 
model.volha3<-lm(vol.ha~ba.ha+stems.ha+qdbh+age+si)  
anova(model.volha,model.volha3)                # partial F test to compare the two nested models
model.volha4<-lm(vol.ha~ba.ha+stems.ha+qdbh+age+topht)  
anova(model.volha,model.volha4)                # partial F test to compare the two nested models


##IV. Given stems/ha=300, qdbh=20 cm, and ba/ha=20 m2/ha, what is the estimated volume per ha?  How would you get a CI for this estimate? 
model.volha<-lm(vol.ha~ba.ha+stems.ha+qdbh)  
predict(model.volha,data.frame(stems.ha=300, ba.ha=20, qdbh=20),interval="confidence")
predict(model.volha,data.frame(stems.ha=300, ba.ha=20, qdbh=20),interval="prediction")

# get 95% confidence intervals for the average vol/ha, given each of the stems/ha in the data, using the equation
predCI<- predict(model.volha, standdat,interval="confidence")
predCI


# plot the orginal data, the predicted values and 95% confidence interval bands
rank<- order(stems.ha)
sortedpredCI=predCI[rank,]
stems.ha.sort<-stems.ha[rank]
plot(stems.ha,vol.ha)
matlines(stems.ha.sort,sortedpredCI,lty=c(1,2,2)) #plot the columns of one matrix against the columns of anothergiven



