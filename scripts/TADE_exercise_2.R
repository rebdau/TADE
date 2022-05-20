##Exercise 2
#significant difference between male and female students for Pulse, Height, and Age?

library(MASS)
names(survey)
is.factor(survey$Sex)
sex <- survey$Sex

#we create a function that extracts the interesting values from the t.test
#that function we will be able to use it on different variables by sapply
#first we analyse the result of the t.test to find how to get to these values
tresult <- t.test(survey$Pulse ~ sex)
names(tresult)
tresult$statistic
tresult$p.value
tresult$estimate
result[c("estimate","p.value","statistic")]
#this is ok but they are in a list
#I would like them in a nice table
class(tresult[c("estimate","p.value","statistic")])
unlist(tresult[c("estimate","p.value","statistic")])
is.vector(unlist(tresult[c("estimate","p.value","statistic")])) #I hope it's a vector!
#this vector outcome from the t.test I will be able to bind together with vector outcome of other t.tests in a table

#now I'm ready to create the function I need
tt <- function (data){
tresult <- t.test(data ~ sex)
unlist(tresult[c("estimate","p.value","statistic")])
}

#let's test the function on the pulse data
tt(survey$Pulse) #hope it works!

#now let's apply it to all 3 variables and see how it looks
sapply(survey[,c("Pulse","Height","Age")], tt)

#great! but it would look even better the other way around: transpose
t(sapply(survey[,c("Pulse","Height","Age")], tt))

##Answer: only for the height there is a significant difference between male and female students at significance level 0.05.
#For pulse and age there is no significant difference between male and female students at significance level 0.05.


