##############################################################################################
#
# bring in data from an outside comma delimited text file, can be converted from a EXCEL file
#
##############################################################################################

plotdat <-  read.csv("C:\\path\\to\\your\\file\\crd.csv",header=TRUE)

names(plotdat)
dim(plotdat)

attach(plotdat)
tapply(AveDbh,Treatment,mean)


##########################################################################
#  Box plots 
#########################################################################

boxplot(AveDbh~Treatment,data=plotdat,col='pink', main="Average Dbh by Treatment")

##########################################################################
# ANOVA: Make sure the treatment is a factor!
# Use lm to get the analysis of variance table -- and F test for treatments
#########################################################################

model1<-lm(AveDbh~as.factor(Treatment),data=plotdat)
anova(model1)#anova approach
#alternative functions:
aov(model1)
aov(AveDbh~as.factor(Treatment),data=plotdat)

summary(model1)#dummy-coded regression approach


##########################################################################
# Check assumptions of equal variance across treatments, and normality
# of residuals.  Residual here is the observed value - mean for the 
# treatment.  
# Set this up for three plots on a page.  After, if you just highlight and run one plot,
# you will just get that plot.  
#########################################################################

yhat<-fitted(model1)
resid<-resid(model1)

par(mfrow=c(2,2))
plot(resid~yhat)  # residual plot
qqnorm(resid)     # normality plot
qqline(resid,col=2)
hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1))


##########################################################################
# If treatment means vary, then test which pairs differ
#########################################################################

pairwise.t.test(AveDbh,Treatment,p.adj="bonferroni")# bonferroni correstion: p-values are multiplied by the number of comparisons

#############################################################################################
#  If assumptions were not met (the graphs show the residuals are not normally 
#  distributed, and/or the residual plot indicates the spread of residuals is
#  not all the same), then repeat the ANOVA using a transformed y-variable instead.  
#############################################################################################

##########################################################################
#  
# Transformations 
# The highest p-value in the Bartlett test corresponds to the highest homoscedasticity
# You can do a Bartlett test for different transformations and 
# select the transformation that resulted in the highest p-value for the Bartlett test
#
#########################################################################

logAveDbh <- log(AveDbh)
invAveDbh <- AveDbh^(-1)
inv_sqrtAveDbh <- AveDbh^(-0.5)
sqrtAveDbh <- AveDbh^(0.5)
transformations <- cbind(logAveDbh, invAveDbh, inv_sqrtAveDbh, sqrtAveDbh, AveDbh)


bart<-c()
bart[1]<-bartlett.test(logAveDbh~as.factor(Treatment),data=plotdat)$p.value
bart[2]<-bartlett.test(invAveDbh~as.factor(Treatment),data=plotdat)$p.value
bart[3]<-bartlett.test(inv_sqrtAveDbh~as.factor(Treatment),data=plotdat)$p.value
bart[4]<-bartlett.test(sqrtAveDbh~as.factor(Treatment),data=plotdat)$p.value
bart[5]<-bartlett.test(AveDbh~as.factor(Treatment),data=plotdat)$p.value
trans<-match(max(bart),bart)

best <- transformations[,trans]
model1<-lm(best~as.factor(plotdat$Treatment))

resid<-resid(model1)
plot(resid~yhat)  # residual plot


