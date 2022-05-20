ufc <- read.csv("../data/ufc_processed.csv")
#In this dataset we will check if species helps us in the prediction of height
names(ufc)
table(ufc$species)
#lets reduce the dataset to the species DF (Douglas Fir), WC and GF
threespec <- na.omit(ufc[ufc$species %in% c("DF","WC","GF"),])
plot(threespec$height[threespec$species %in% "DF"]~threespec$dbh[threespec$species %in% "DF"],pch="D", xlab="dbh", ylab="height")
points(threespec$height[threespec$species %in% "WC"]~threespec$dbh[threespec$species %in% "WC"],pch="W",col="red")
points(threespec$height[threespec$species %in% "GF"]~threespec$dbh[threespec$species %in% "GF"],pch="G",col="green")

#represent a categorical variable numerically using 
#2 dummies for 3 species
threespec$x1 <- 0
threespec$x2 <- 0
threespec$x1[threespec$species=="WC"] <- 1
threespec$x2[threespec$species=="GF"] <- 1
#all dummies are zero for DF

threespec$x3 <- log10(threespec$dbh)
threespec$x4 <- threespec$dbh
threespec$y <- log(threespec$height)

#interactions are formed by the product of two or more predictors
threespec$x5 <- threespec$x1 * threespec$x3
threespec$x6 <- threespec$x2 * threespec$x3
threespec$x7 <- threespec$x1 * threespec$x4
threespec$x8 <- threespec$x2 * threespec$x4

#full model with intercept and slope varying by species
attach(threespec)
full <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8)

#reduced model with one common equation regardless of species
common <- lm(y~x3+x4)

#reduced model with common slopes for all species, but different intercepts
intonly <- lm(y~x1+x2+x3+x4)

##are the assumptions met?
#Full
yhat<-fitted(full)
resid<-resid(full)

par(mfrow=c(2,2),cex=0.7)
plot(yhat~y)
plot(resid~yhat)  # residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1),cex=1)

#Common
yhat<-fitted(common)
resid<-resid(common)

par(mfrow=c(2,2),cex=0.7)
plot(yhat~y)
plot(resid~yhat)  # residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1),cex=1)

#Intercept Only
yhat<-fitted(intonly)
resid<-resid(intonly)

par(mfrow=c(2,2),cex=0.7)
plot(yhat~y)
plot(resid~yhat)  # residual plot

qqnorm(resid)     # normality plot
qqline(resid,col=2)

hist(resid, breaks =6 , density=10,col="green", border="black") # draws a histogram
par(mfrow=c(1,1),cex=1)

##Rsquared and SEE
#Full
names(summary(full))
SEE <- summary(full)$sigma
rsq <- summary(full)$r.squared
adjr2 <- summary(full)$adj.r.squared
cbind(SEE, rsq, adjr2)
#Common
SEE <- summary(common)$sigma
rsq <- summary(common)$r.squared
adjr2 <- summary(common)$adj.r.squared
cbind(SEE, rsq, adjr2)
#Intercept Only
EE <- summary(intonly)$sigma
rsq <- summary(intonly)$r.squared
adjr2 <- summary(intonly)$adj.r.squared
cbind(SEE, rsq, adjr2)

##Df, SSR, SSE
#Full
anovatable <- anova(full)
df.model.f <- sum(anovatable[1:8,1])
ssr.f <- sum(anovatable[1:8,2])
df.error.f <- anovatable[9,1]
sse.f <- anovatable[9,2]
full.anova <- cbind(df.model.f,ssr.f,df.error.f,sse.f)

#Common
anovatable <- anova(common)
df.model.c <- sum(anovatable[1:2,1])
ssr.c <- sum(anovatable[1:2,2])
df.error.c <- anovatable[3,1]
sse.c <- anovatable[3,2]
common.anova <- cbind(df.model.c,ssr.c,df.error.c,sse.c)

#Intercept Only
anovatable <- anova(intonly)
df.model.io <- sum(anovatable[1:4,1])
ssr.io <- sum(anovatable[1:4,2])
df.error.io <- anovatable[5,1]
sse.io <- anovatable[5,2]
intonly.anova <- cbind(df.model.io,ssr.io,df.error.io,sse.io)

rbind(full.anova, common.anova, intonly.anova)

partialF <- ((ssr.f-ssr.c)/(df.model.f-df.model.c))/(sse.f/df.error.f)
#critical F-value is 1-alpha percentile with r and n-m-1 (full model) degrees of freedom
df1 <- df.model.f-df.model.c
df2 <- df.error.f
Fc <- qf(0.95,df1, df2)
#Decision: if partialF > critical F, then we reject the H0 that species are NOT contributing in predicting the variable (have slope = 0)
#in other words, we decidede that species are contributing
partialF>Fc

anova(common,full)#check if I calculated it correctly

###Could we use the same slope, just different intercepts?
#FUll vs intercepts only model
#H0: slopes are the same for all species
#H1: slopes differ
partialF <- ((ssr.f-ssr.io)/(df.model.f-df.model.io))/(sse.f/df.error.f)
df1 <- df.model.f-df.model.io
df2 <- df.error.f
Fc <- qf(0.95,df1, df2)
partialF>Fc
anova(intonly,full)


###Are the differences in intercept significant between the species?
#intercepts only vs common model
#H0: intercepts are the same for all species
#H1: intercepts differ
partialF <- ((ssr.io-ssr.c)/(df.model.io-df.model.c))/(sse.io/df.error.io)
df1 <- df.model.io-df.model.c
df2 <- df.error.io
Fc <- qf(0.95,df1, df2)
partialF>Fc
anova(common,intonly)


##there is a faster way in R, if you make sure that the categorical variable is a factor:
detach(threespec)
threespec <- na.omit(ufc[ufc$species %in% c("DF","WC","GF"),])
names(threespec)
is.factor(threespec$species)
attach(threespec)
y <- log(height)
logdbh <- log10(dbh)
model <- lm(y~dbh+logdbh+species)
model2 <- lm(y~dbh+logdbh)

anova(model2,model)#Partial F for species
anova(model)#Partial F for species

##this is the same result like anova(common,intonly)
##this means that R just tests the intercepts, not the slopes!!

summary(model)$coefficients
summary(intonly)$coefficients
#The coefficients of these two models are the same
#the coefficients speciesGF and speciesWC correspond to the coefficients of the dummy-variables









