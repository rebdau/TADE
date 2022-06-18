# Fish density and stream slope.

# A series of streams were randomly selected. 
# Two sites in each stream were selected and
# the slope of the stream bed and the areal fish 
# density (per sq meter) were measured.

fish <- read.csv("paired_stream.csv", header=TRUE, as.is=TRUE)
fish[1:12,]

#*************************************************************************
# Analyze the differences

# We need to split the data set. 
# Extract the columns and then merge on matching variables.

low.slope  <- fish[ fish$slopeclass == "low" ,]
names(low.slope)[names(low.slope)=="density"]<-"low.density"
high.slope <- fish[ fish$slopeclass == "high",]
names(high.slope)[names(high.slope)=="density"]<-"high.density"

paired.slope <- merge(low.slope [,c("stream","low.density")], 
                      high.slope[,c("stream","high.density")], by="stream")
paired.slope

# Do a plot to see if there are any outliers
plot(paired.slope$low.density, paired.slope$high.density,
     main="Check for outliers",
     xlab="Low Density", ylab="High Density")

# Compute the difference between the densities

paired.slope$diff <- paired.slope$low.density - paired.slope$high.density
paired.slope

# Get dot plot to see if any outliers 
#stripchart produces one dimensional scatter plots (or dot plots) of the given data. 
stripchart(paired.slope$diff,  
   vertical=TRUE, method="jitter", jitter=.1,  
   main="Difference (low.density-high.density) of density", 
   xlab="", ylab="Diff density (low.density-high.density)")
abline(h=0, lty=2)

t.test(paired.slope$diff)

t.test(paired.slope$low.density, paired.slope$high.density, paired=TRUE)

#*************************************************************************
# Analyze as a linear model using  lm()
fish <- read.csv("paired_stream.csv", header=TRUE, as.is=TRUE)


# declare factor and blocking variables are factors
fish$slopeclass  <- factor(fish$slopeclass)
fish$stream       <- factor(fish$stream)
str(fish)

model <- lm(density ~ stream + slopeclass, data=fish)
anova(model)

# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means 
library(lsmeans)
model.lsmo <- lsmeans(model, ~slopeclass, adjust="tukey")
summary(model.lsmo, infer=TRUE)

# Find all the pairwise differences adjusting for multipicity
model.pairs <- pairs(model.lsmo, adjust="tukey")
summary(model.pairs, infer=TRUE)


