# Circle.Utilities.R
# Author: Yuchen Zhou
# the script contains all functions for the test statistic

# the function takes 1 argument k, a vector of k numeric values which represent the angles which the birds have left bird feeder
# the function calculates and returns the maximum difference between sequential angles
max.angular.diff <- function(k){
	
	# sort the angles and create a vector to store the difference between sequential angles
	sortedangle <- sort(k)
	angle.indices <- seq_along(sortedangle)
	
	# find the differences between consecutive value
	angledifference <-diff(sortedangle)
	angledifference <- append(angledifference,value=2*pi-sortedangle[length(sortedangle)]+sortedangle[1])
		
	# the maximum difference is returned by the function
	maxangulardiff <- max(angledifference)
	return(maxangulardiff)	
}

# the function takes 2 arguments: k an integer which indicates how many birds are in a single batch of measurement
# n is an integer which indicates the number of time we sample our data
# the function will return the n calculated maximum angular difference
sim.null.hypo <- function(k,n){
	
	# create a vector to store all calculated maximum angular difference
	allmaxangulardiff <- c()
	
	# loop n times to sample the data from a uniform distribution which is the appropriate distribution for this assignment
	for (index in 1:n){
		# sample k values from uniform distribution which has a minimum value of 0 and a maximum value of 2*pi
		mydata <- runif(k,min=0,max=2*pi)
		# utilize max.angular.diff to calculate maximum angular different for the sample
		allmaxangulardiff[index] <- max.angular.diff(mydata)
	}
	
	return (allmaxangulardiff)
}

# the function takes a single argument, a vecotr of n numeric values which represent maximum angular differences
# the function calculates the cumulative distribution function for the distribution which represent the data
calc.cdf <- function(n){
	
	# use hist to get the cumulative distribution function
	histoutput <- hist(n,plot=FALSE,breaks=40)
	cumulativedistributionfunction <- cumsum(histoutput$counts)/sum(histoutput$counts)
	# create the dataframe using the output of hist
	cdf.dataframe <- data.frame(histoutput$breaks[-1],cumulativedistributionfunction)
	colnames(cdf.dataframe) <- c("breaks","Cumulative.data")
	return (cdf.dataframe)
}

# the function takes 2 argument. One is a data frame which contains the columns breaks and Cumulative.Data
# the second argument is a single numeric value
# the function will determine which bin the single values is in and return the value of the cumulative distribution function for that bin
calc.cumulative <- function(mydataframe,binvalue){
	
	# slice the dataframe so only the entries with bin values that are less or equal to the specific single value are stored in binvalue.lessandequal
	binvalue.lessandequal <- mydataframe[mydataframe$breaks<=binvalue,]
	# get the entry with the maximum bin value in binvalue.lessandequal. That entry is the bin that contains the single value
	binvalue.isin <- binvalue.lessandequal[binvalue.lessandequal$breaks==max(binvalue.lessandequal$breaks),]
	# get the cumulative distribution function for that bin
	return (binvalue.isin$Cumulative.data)
	
}
