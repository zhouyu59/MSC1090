# test.bird.data.R
# Author: Yuchen Zhou
# The script will perform test statistic on the bird data

# load the library EnvStats
library("EnvStats")

# get the command line argument
numberofdata <- commandArgs(trailingOnly=TRUE)

# check whether the command line argument is valid
if (length(numberofdata)!=1 || is.na(as.numeric(numberofdata))){
	cat("please enter one number as the command line argument\n")
	quit()
}

# load the utility file
source("Circle.Utilities.R")

# convert the argument to a number
numberofdata.number=as.numeric(numberofdata[1])
# output the argument
cat("The number of data points to include in each batch of measured birds is ",numberofdata.number,".\n")

# generate our data using rtri
my.data <-rtri(numberofdata.number,0.5*pi,1.5*pi,mode=pi)

# generate null hypothesis data
nullhypothesis.values <- sim.null.hypo(numberofdata,10000)
nullhypothesis.cdf <- calc.cdf(nullhypothesis.values)

# shapiro.test shows that the data are normally distributed so we can perform unpaired t-test

# performed unpaired t test and store the test result
result<- t.test(my.data,nullhypothesis.cdf)

# output the result of the t test
if(result$p.value<0.05){
	cat("The test is significant, with a p value of ",result$p.value,".\n")
}else{
	cat("The test is not significant, with a p value of", result$p.value,".\n")
}


