##############################################################################################
#
# bring in data from an outside comma delimited text file, can be converted from a EXCEL file
#
##############################################################################################

plotdat <-  read.csv("C:\\path\\to\\your\\file\\crd_two_factors.csv",header=TRUE)
names(plotdat)
dim(plotdat)

attach(plotdat)

logADbh=log(AveDbh)


# Create a class variable to represent Fertilizer
FertLevel<- factor(FertLevel)

# Create a label at the Treatment level
Treatment <-array(0, dim=c(12,1))

plotdat2<- data.frame(plotdat,FertLevel,Treatment,logADbh)
plotdat2

detach(plotdat)

rm(plotdat,FertLevel,Treatment,logADbh)

attach(plotdat2)

# reset Treatment to indicates each treatment j=1 till j=(2*3)=6 with distinct number
j<-1
test1<-as.numeric(FertLevel.1==1 & Thinning=="none")
Treatment <- (j*test1)+Treatment
j<-2
test1<-as.numeric(FertLevel.1==2 & Thinning=="none")
Treatment <- (j*test1)+Treatment
j<-3
test1<-as.numeric(FertLevel.1==1 & Thinning=="light")
Treatment<- (j*test1)+Treatment
j<-4
test1<-as.numeric(FertLevel.1==2 & Thinning=="light")
Treatment<- (j*test1)+Treatment
j<-5
test1<-as.numeric(FertLevel.1==1 & Thinning=="heavy")
Treatment<- (j*test1)+Treatment
j<-6
test1<-as.numeric(FertLevel.1==2 & Thinning=="heavy")
Treatment<- (j*test1)+Treatment
Treatment<- factor(Treatment)

plotdat3<-data.frame(plotdat2,Treatment)

detach(plotdat2)
rm(plotdat2,j,test1,Treatment)

plotdat3
attach(plotdat3)


##########################################################################
#  Means by factor, and then by Treatment which is the same as by
#  crossed factor levels 
#########################################################################

tapply(AveDbh,Thinning,mean)
tapply(AveDbh,FertLevel.1,mean)
tapply(AveDbh,Treatment.1,mean)
tapply(AveDbh,list(Thinning,FertLevel.1),mean)
tapply(AveDbh,list(Thinning,FertLevel.1),sd)

tapply(logADbh,Thinning,mean)
tapply(logADbh,FertLevel.1,mean)
tapply(logADbh,Treatment.1,mean)
tapply(logADbh,list(Thinning,FertLevel.1),mean)
tapply(logADbh,list(Thinning,FertLevel.1),sd)

##########################################################################
#  Box plots, one orginal and one log-transformed.  Set this up for two
#  plots on a page and reduce the text size to 0.8 times.  
#  If you just highlight and run one boxplot,
#  you will just get that plot.   
#########################################################################

par(mfrow=c(1,2),cex=0.8)
boxplot(AveDbh~Treatment.1,data=plotdat3,col='pink', main="Plot AveDbh by Treatment")
boxplot(logADbh~Treatment.1,data=plotdat3,col='pink', main="Log AveDbh by Treatment")
par(mfrow=c(1,1),cex=1)

# interaction plot #
par(mfrow=c(2,1),cex=0.8)
interaction.plot(Thinning,FertLevel.1,AveDbh)
interaction.plot(Thinning,FertLevel.1,logADbh)
par(mfrow=c(1,1),cex=1)

##########################################################################
# ANALYSIS 1: Using AveDbh  
# Use lm to get the analysis of variance table -- F test for
# interactions, and main effects.  If interactions NOT significant
# then we can assess main effects.  Otherwise, the assessment
# is at the treatment level (crossed factors).
# Check assumptions of equal variance across treatments, and normality
# of residuals.  Residual here is the observed value - mean for the 
# treatment.  
#########################################################################

model1<-lm(AveDbh~Thinning+FertLevel.1+Thinning*FertLevel.1,data=plotdat3)
anova1<- anova(model1)
anova1
summary(model1)#regression approach with 5 dummies (for six different treatments)

yhat<-fitted(model1)#these are the treatment means
resid<-resid(model1)

par(mfrow=c(2,2))
plot(resid~yhat)  # residual plot
qqnorm(resid)     # normality plot
qqline(resid,col=2)
hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1))

##########################################################################
# If the interaction is significant, then test which pairs of treatments
# differ  # NOTE:  Remember to adjust alpha using (bonferroni correction)
# Alternative: pairwise.t.test(AveDbh,Treatment.1,p.adj="bonferroni")
#  does NOT use correct MSE for two factors
#########################################################################

####  Correct pairwise t-tests if there are interactions  ##

fit.dat<- cbind(Treatment.1,yhat)
MSE<- anova1$"Mean Sq"[4]#we have to use the MSE of the two-way anova table
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)#the treatment means
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)#number of experimental units per treatment
ntreat<-length(lsmeans)#number of treatments
dfError<- anova1$Df[4]

# Calculate the standard errors of pairs #
sd<- array(0,c(ntreat,ntreat))
i<- 1
repeat {
sd[i,1:ntreat] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:ntreat])) )^0.5
i<- i+1
if (i >ntreat) break }
sd

#  set up arrays for t values and p values #
tvalues<- array(0,c(ntreat,ntreat))
pvalues<- array(0,c(ntreat,ntreat))

#calculate pairs of means t-values #
i<- 1
repeat {tvalues[i,1:ntreat]<- (lsmeans[i]-lsmeans[1:ntreat])/sd[i,1:ntreat]
i<- i+1
if (i > ntreat) break}
tvalues

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:ntreat]<- 2*(pt(abs(tvalues[i,1:ntreat]),dfError,lower.tail=FALSE))
i<- i+1
if (i > ntreat) break}
pvalues

#significant after correction of alpha level?
alpha <- 0.05
npairs <- choose(ntreat,2)#number of pairs of means to compare
alpha_corrected <- alpha/npairs
pvalues<alpha_corrected

rm(fit.dat,nsamples,sd,tvalues,pvalues,ntreat,lsmeans,dfError,
i,MSE,alpha,npairs,alpha_corrected)
##########################################################################
# If treatment interaction was NOT significant, then test which pairs 
# of levels for main effects differ. Must correct alpha by dividing by
# number of pairs
#########################################################################

#  Tests for Thinning (factor 1) #
fit.dat<- cbind(Thinning,yhat)
MSE<- anova1$"Mean Sq"[4]
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)
nfactor1<-length(lsmeans)#number of levels in factor 1
dfError<- anova1$Df[4]

# Calculate the standard errors of pairs #
sd<- array(0,c(nfactor1,nfactor1))
i<- 1
repeat {
sd[i,1:nfactor1] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:nfactor1])) )^0.5
i<- i+1
if (i >nfactor1) break }
sd

#  set up arrays for t values and p values #
tvalues<- array(0,c(nfactor1,nfactor1))
pvalues<- array(0,c(nfactor1,nfactor1))

#calculate pairs of means t-values #
i<- 1
repeat {tvalues[i,1:nfactor1]<- (lsmeans[i]-lsmeans[1:nfactor1])/sd[i,1:nfactor1]
i<- i+1
if (i > nfactor1) break}
tvalues

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:nfactor1]<- 2*(1-pt(abs(tvalues[i,1:nfactor1]),dfError))
i<- i+1
if (i > nfactor1) break}
pvalues

#significant after correction of alpha level?
alpha <- 0.05
npairs <- choose(nfactor1,2)#number of pairs of means to compare
alpha_corrected <- alpha/npairs
pvalues<alpha_corrected

rm(fit.dat,nsamples,sd,tvalues,pvalues,nfactor1,lsmeans,dfError,
i,MSE, alpha, npairs, alpha_corrected)

#  Tests for FertLevel.1 (factor 2) #
fit.dat<- cbind(FertLevel.1,yhat)
MSE<- anova1$"Mean Sq"[4]
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)
nfactor2<-length(lsmeans)
dfError<- anova1$Df[4]

# Calculate the standard errors of pairs #
sd<- array(0,c(nfactor2,nfactor2))
i<- 1
repeat {
sd[i,1:nfactor2] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:nfactor2])) )^0.5
i<- i+1
if (i >nfactor2) break }
sd

#  set up arrays for t values and p values #
tvalues<- array(0,c(nfactor2,nfactor2))
pvalues<- array(0,c(nfactor2,nfactor2))

#calculate pairs of means t-values #
i<- 1
repeat {tvalues[i,1:nfactor2]<- (lsmeans[i]-lsmeans[1:nfactor2])/sd[i,1:nfactor2]
i<- i+1
if (i > nfactor2) break}
tvalues

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:nfactor2]<- 2*(1-pt(abs(tvalues[i,1:nfactor2]),dfError))
i<- i+1
if (i > nfactor2) break}
pvalues

#significant after correction of alpha level?
alpha <- 0.05
npairs <- choose(nfactor2,2)#number of pairs of means to compare
alpha_corrected <- alpha/npairs
pvalues<alpha_corrected

rm(fit.dat,nsamples,sd,ttest,pvalues,nfactor2,lsmeans,dfError,
i,MSE,anova1,model1,yhat,resid, alpha, npairs, alpha_corrected)


##########################################################################
# ANALYSIS 2: Using logADbh  
# Use lm to get the analysis of variance table -- F test for
# interactions, and main effects.  If interactions NOT significant
# then we can assess main effects.  Otherwise, the assessment
# is at the treatment level (crossed factors).
# Check assumptions of equal variance across treatments, and normality
# of residuals.  Residual here is the observed value - mean for the 
# treatment.  
#########################################################################

model1<-lm(logADbh~Thinning+FertLevel.1+Thinning*FertLevel.1,data=plotdat3)
anova1<- anova(model1)
anova1

summary(model1)

yhat<-fitted(model1)
resid<-resid(model1)

par(mfrow=c(2,2))
plot(resid~yhat)  # residual plot
qqnorm(resid)     # normality plot
qqline(resid,col=2)
hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogramresid<-resid(lm.model1)
par(mfrow=c(1,1))

##########################################################################
# If the interaction is significant, then test which pairs of treatments
# differ  # NOTE:  Remember to adjust alpha using (bonferroni correction)
# Alternative: pairwise.t.test(logADbh,Treatment.1,p.adj="bonferroni")
#  does NOT use correct MSE for two factors
#########################################################################

####  Correct pairwise t-tests if there are interactions  ##

fit.dat<- cbind(Treatment.1,yhat)
MSE<- anova1$"Mean Sq"[4]
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)
cbind(lsmeans,nsamples)
ntreat<-length(lsmeans)
dfError<- anova1$Df[4]
cbind(ntreat,dfError)

# Calculate the standard errors of pairs #
sd<- array(0,c(ntreat,ntreat))
i<- 1
sum<- 0.0
repeat {
sd[i,1:ntreat] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:ntreat])) )^0.5
i<- i+1
if (i >ntreat) break }
sd

#  set up arrays for t values and p values #
ttest<- array(0,c(ntreat,ntreat))
pvalues<- array(0,c(ntreat,ntreat))

#calculate pairs of means t-values #
i<- 1
repeat {ttest[i,1:ntreat]<- (lsmeans[i]-lsmeans[1:ntreat])/sd[i,1:ntreat]
i<- i+1
if (i > ntreat) break}
ttest

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:ntreat]<- 2*(1-pt(abs(ttest[i,1:ntreat]),dfError))
i<- i+1
if (i > ntreat) break}
pvalues

rm(fit.dat,nsamples,sd,ttest,pvalues,ntreat,lsmeans,dfError,
i,sum,MSE)
##########################################################################
# If treatment interaction was NOT significant, then test which pairs 
# of levels for main effects differ. Must correct alpha by dividing by
# number of pairs
#########################################################################

#  Tests for Thinning (factor 1) #
fit.dat<- cbind(Thinning,yhat)
MSE<- anova1$"Mean Sq"[4]
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)
cbind(lsmeans,nsamples)
nfactor1<-length(lsmeans)
dfError<- anova1$Df[4]
cbind(nfactor1,dfError)

# Calculate the standard errors of pairs #
sd<- array(0,c(nfactor1,nfactor1))
i<- 1
sum<- 0.0
repeat {
sd[i,1:nfactor1] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:nfactor1])) )^0.5
i<- i+1
if (i >nfactor1) break }
sd

#  set up arrays for t values and p values #
ttest<- array(0,c(nfactor1,nfactor1))
pvalues<- array(0,c(nfactor1,nfactor1))

#calculate pairs of means t-values #
i<- 1
repeat {ttest[i,1:nfactor1]<- (lsmeans[i]-lsmeans[1:nfactor1])/sd[i,1:nfactor1]
i<- i+1
if (i > nfactor1) break}
ttest

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:nfactor1]<- 2*(1-pt(abs(ttest[i,1:nfactor1]),dfError))
i<- i+1
if (i > nfactor1) break}
pvalues

rm(fit.dat,nsamples,sd,ttest,pvalues,nfactor1,lsmeans,dfError,
i,sum,MSE)

#  Tests for FertLevel.1 (factor 2) #
fit.dat<- cbind(FertLevel.1,yhat)
MSE<- anova1$"Mean Sq"[4]
lsmeans<- tapply(fit.dat[,2],fit.dat[,1],mean)
nsamples<- tapply(fit.dat[,2],fit.dat[,1],length)
cbind(lsmeans,nsamples)
nfactor2<-length(lsmeans)
dfError<- anova1$Df[4]
cbind(nfactor2,dfError)

# Calculate the standard errors of pairs #
sd<- array(0,c(nfactor2,nfactor2))
i<- 1
sum<- 0.0
repeat {
sd[i,1:nfactor2] <- (MSE*( (1/nsamples[i])+(1/nsamples[1:nfactor2])) )^0.5
i<- i+1
if (i >nfactor2) break }
sd

#  set up arrays for t values and p values #
ttest<- array(0,c(nfactor2,nfactor2))
pvalues<- array(0,c(nfactor2,nfactor2))

#calculate pairs of means t-values #
i<- 1
repeat {ttest[i,1:nfactor2]<- (lsmeans[i]-lsmeans[1:nfactor2])/sd[i,1:nfactor2]
i<- i+1
if (i > nfactor2) break}
ttest

# calculate pairs of means p-values, no correction #
i<- 1
repeat {pvalues[i,1:nfactor2]<- 2*(1-pt(abs(ttest[i,1:nfactor2]),dfError))
i<- i+1
if (i > nfactor2) break}
pvalues

rm(fit.dat,nsamples,sd,ttest,pvalues,nfactor2,lsmeans,dfError,
i,sum,MSE,model1,anova1,yhat,resid)

#############################################################################################
#  Clean up all of your files, or shut down R before doing another exercise
#############################################################################################
detach(plotdat3)
rm(plotdat3)


