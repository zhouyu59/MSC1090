# driverScript.R
# Author: Yuchen Zhou
# This script perform nonparametric bootstrapping for different values of K. It plots the value of the test statistics versus k. 

# use ggplot2 to draw graph
library("plotrix")

# load the utility file
source("NewUtilities.R")

# get the command line argument
args <- commandArgs(trailingOnly=TRUE)

# check whether the number of command line argument is valid
if(length(args)!=1){
	cat ("The script requires one argument, indicating the maximum number of k to evaluate.\n")
	quit()
}

# check whether the command line argument is an integer
if (is.na(as.numeric(args))){
	cat("The argument needs to be a number.\n")
	quit()
}

# run boot.sim m=1000, n=2000
results <- boot.sim(as.numeric(args[1]),1000,2000)

# draw the graph with confidence interval
plotCI(x=2:as.numeric(args[1]),y=results[,1],li=results[,3],ui=results[,2],xlab="k",ylab="Most-probable Value")
