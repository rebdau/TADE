#Exercice: find the confidence intervals for the mean for all numeric values in the ufc dataset
#we create a function to print the limits of the confidence interval
CI <- function (data) { #it's going to be a function in which the only parameter to  specify will be "data"
tt <- t.test(data)
tt$conf.int[1:2]
 }
#we can test the function on one variable
CI(ufc$height)	#does it work? ok!
#we apply it to all numeric variable
#how can I find automatically the numeric variables?
sapply(ufc,class)
numericvariables <- sapply(ufc,class)=="numeric"	#I make a logical vector indicating the numeric variables
#or if I wanted to include also the integers:
#numericvariables <- sapply(ufc,class)=="numeric"|sapply(ufc,class)=="integer"
#on these numeric variables I apply my function:
sapply(ufc[,numericvariables],CI)

