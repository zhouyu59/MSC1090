# NewUtilities.R
# Author: Yuchen Zhou
# the R script contains all new utilities functions that are needed to perform bootstrapping

# import nptest library
library(boot)

# load the utility file
source("Circle.Utilities.R")


# The function returns the most probable value from a vector of numeric value
# n is a vector of numeric values . i is a vector of index.
most.probable.value <- function(mydata,i){
	
	histoutput <- hist(mydata[i],plot=FALSE,breaks=1000)
	
	# get the index with the most counts
	highestcountindex <- which(histoutput$counts==max(histoutput$counts))
	
	# return the midpoint of that index
	mostprobablevalue <- histoutput$mids[highestcountindex[1]]
	return(mostprobablevalue)
}

# the function takes a vector of numeric values, and an interger n as arugments. The function will perform nonparametric bootstrapping n times and calculate the most probable value. The function
# will return the ouput of the boot command
nonpara.boot <- function(v, n){
	myboot <- boot(data=v,statistic=most.probable.value,R=n)
	return(myboot)
}

# The function returns a matrix that contains the test statisticss, the upper bounds of the confidence intervsls, and the lower bounds of the confidence intervals
# k is the maximum number of birds in a single batch of measurement. m is the number of times we sample the data. n is the number of bootstraps
boot.sim <- function(k,m,n){
	
	# create three empty vectors that will be used to store value of statistics, upper bound, and lower bound values
	statisticsvector <- c()
	upperintervalvector <- c()
	lowerintervalvector <- c()
	
	# loop through the possible values of k
	for (i in 2:k){
		
		# simulate data using sim.null.hypo
		simdata<-sim.null.hypo(i,m)
		
		# rung nonpara.boot function of the data
		bootresult <- nonpara.boot(simdata,n)
		statisticsvector <- append(statisticsvector,c(bootresult$t0))
		
		# get the confidence interval
		ciresult <- boot.ci(bootresult,conf=0.95, type="bca")
		lowerbound <- ciresult$bca[4]
		upperbound <- ciresult$bca[5]
		
		# store the values in the vectors
		upperintervalvector <- append(upperintervalvector,upperbound)
		lowerintervalvector <- append(lowerintervalvector,lowerbound)		
	} 
	
	# return the matrix that contains values of statistics, upper bound, and lower bound
	return(cbind(statisticsvector,upperintervalvector,lowerintervalvector))
}