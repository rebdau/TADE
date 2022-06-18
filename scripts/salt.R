# Effect of salt on biomass
# An experiment was conducted where different amounts 
# of salt (ppm) were added to plots and the resulting 
# biomass of grass was measured. The experiment was 
# replicated in four blocks.
#


marsh <- read.csv("salt.csv", header=TRUE, as.is=TRUE)
print(marsh[1:12,])

# Declare both block and trt variable  as factors
marsh$salt   <- factor(marsh$salt)
marsh$block  <- factor(marsh$block)
str(marsh)




# Get side-by-side dot plots 
boxplot( biomass ~ salt, data=marsh, range=0,
   main="biomass at various salts", 
   sub="Whiskers extend to range of data",
   xlab="sal", ylab="biomass (g)")

stripchart(biomass ~ salt, data=marsh, add=TRUE, 
     vertical=TRUE, method="jitter", jitter=.1)



# Profile plot 
# Do a plot of the biomass over the salts by sample to check for additivity
stripchart(biomass ~ salt, data=marsh, vertical=TRUE)

library(plyr)

d_ply(marsh,"block", 
    function(x){points(as.numeric(x$salt), x$biomass, type="l")})
#d_ply: For each subset of a data frame, apply function and discard results   


# Compute some summary statistics for each group 
#first create the function "sf.simple.summary" using "helpoerfunctions.R"
report <- ddply(marsh, "salt", sf.simple.summary, variable="biomass")
report


# fit the linear model and get the ANOVA table and test for effects
# Note that if the design is UNbalanced (some missing data), we must fit the block term 
# first and then the treatment term --

model <- lm(biomass ~ block + salt, data=marsh)
anova(model)



# Check the assumptions of the ANOVA model 
layout(matrix(1:4, nrow=2))
plot(result)
layout(1)


# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means 
library(lsmeans)
model.lsmo <- lsmeans(model, ~salt, adjust="tukey")
#not adjusted for simultaneous coverage:
summary(model.lsmo, infer=TRUE)


#adjusted for simultaneous coverage:
summary(model.lsmo, infer=TRUE, adjust="tukey")

# Get the compact letter display and a plot
model.cld <- cld(model.lsmo)
model.cld


# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(model.cld, variable="salt")
plotcld <- plotcld + 
        xlab("salt")+
        ylab("Mean biomass (with 95% ci)")+
        ggtitle("Comparison of mean biomass with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(model.cld, variable="salt")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("salt")+
        ylab("Mean biomass (with 95% ci)")+
        ggtitle("Comparison of mean biomass with cld")
plotcldb

ggsave(plot=plotcld, file="salt-R-cldbar.png")
ggsave(plot=plotcldb, file="salt-R-cldline.png")


# Find all the pairwise differences adjusting for multipicity
model.pairs <- pairs(model.lsmo, adjust="tukey")
summary(model.pairs, infer=TRUE)


model.pairs.ci <- confint(model.pairs) # extract the ci values
model.pairs.ci
plotdiff <- ggplot(model.pairs.ci, aes(contrast, estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_point(size=4)+
    geom_linerange(size=1.5)+
    geom_abline(interecept=0, slope=0, linetype=2)+
    ylab("Estimated diff and 95% ci")+
    xlab("Contrast")+
    ggtitle("Estimated pairwise differences and ci")
plotdiff

ggsave(plot=plotdiff, file="salt-R-pairdiff.png")


# Same plot using the glht package and the default plotting methods
model.pairs.glht <- as.glht(model.pairs)
model.pairs.glht.ci <-confint(model.pairs.glht) # extract the confint
model.pairs.glht.ci$confint

old.par <- par(mar=c(5,5,4,2)) # adjust the left margin of th eplot
plot(model.pairs.glht)# 
par(old.par)


#--------------------------------------------------------------------------------------
# Extract the simulaneous inference using the base R function TukeyHSD
# This requires an aov() fit
model2 <- aov(biomass ~ block + salt, data=marsh)
mcp <- TukeyHSD(model2, which="salt", ordered=TRUE) # ordered sorts means
mcp
plot(mcp)
abline(v=0, lty=2)

# Using the multcomp package
library(multcomp)
model.tukey     <- glht(model2, linfct = mcp(salt = "Tukey"))
model.tukey.cld <- cld(model.tukey)  # joined line plot

# create the display

model.tukey.cld
old.par <- par(mai=c(1.25,1.25,1.50,1))
plot(model.tukey.cld, 
     xlab="salt",
     ylab="biomass")
title(main="Multiple comparison results", line=1)
par <- par(old.par)



