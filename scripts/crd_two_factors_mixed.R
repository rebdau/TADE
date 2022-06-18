fertdat <- read.csv("C:\\Users\\Rebecca\\Documents\\Cours\\201415\\TADE2015\\Fertilizationtrial_two_factors.csv",header=TRUE)
fertdat$A <- as.factor(fertdat$A)
fertdat$B <- as.factor(fertdat$B)

boxplot(result ~ B*A,col=c("white","lightgray"),data=fertdat)

model1 <- lm(result~A + B + A*B,data=fertdat)
anova(model1)

anova(model1)$"Mean Sq"[1]/anova(model1)$"Mean Sq"[3]#this is the correct F-value for A!!!
library(lme4)
model<-lmer(result~A +(1|B)+(1|A:B),data=fertdat)
anova(model)#this gives the correct F-value for A!!!


#I didn't find how to do this with the lme function of the nlme package#

###################
#
#significance: p-value for F
#or likelihood ratio test
#
#########################

calculate P-value for F-value
df_A <- anova(model1)$Df[1]
df_AB <- anova(model1)$Df[3]
F_A <- anova(model)$"F value"[1]#the correct F-value from mixed effects model
1- pf(F_A, df_A, df_AB)#this gives the area to the right of this percentile value

#compare models using the likelihood ratio test
#two models to compare with each other:
#one with the effect in question, one without the effect
#you have to set REML=FALSE. This changes the likelihood estimator

model<-lmer(result~A +(1|B)+(1|A:B),data=fertdat, REML=FALSE)
model.null <-lmer(result~1 +(1|B)+(1|A:B),data=fertdat, REML=FALSE)
anova(model,model.null)

##########################################################################
# test which pairs of levels for the fixed factor differ. Must correct alpha by dividing by
# number of pairs
#########################################################################

#  Factor A: Fertilization, 3 levels, fixed #

MSAB<- anova(model1)$"Mean Sq"[3]
lsmeans<- tapply(fertdat$result,fertdat$A,mean)
nsamples<- tapply(fertdat$result,fertdat$A,length)
nfactor1<-length(lsmeans)#number of levels in factor 1
dfAB<- anova(model1)$Df[3]

# Calculate the standard errors of pairs #
sd<- array(0,c(nfactor1,nfactor1))
i<- 1
repeat {
sd[i,1:nfactor1] <- (MSAB*( (1/nsamples[i])+(1/nsamples[1:nfactor1])) )^0.5
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
repeat {pvalues[i,1:nfactor1]<- 2*(1-pt(abs(tvalues[i,1:nfactor1]),dfAB))
i<- i+1
if (i > nfactor1) break}
pvalues

#significant after correction of alpha level?
alpha <- 0.05
npairs <- choose(nfactor1,2)#number of pairs of means to compare
alpha_corrected <- alpha/npairs
pvalues<alpha_corrected

rm(model1,model.null,model,nsamples,sd,tvalues,pvalues,nfactor1,lsmeans,dfAB,
i,MSAB, alpha, npairs, alpha_corrected)


