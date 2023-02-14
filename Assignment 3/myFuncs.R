
# the function takes 1 argument x, a vector of numberic values. The function will calculate and return the geometric mean of positive values in the vector x
geom.pos.mean <- function(x){
	
	# select positive numbers and ignore negative numbers
	positivex <- x[(x>0)]
	
	# get the number of positive values in the vector
	numberofpositives <- length(positivex)
	
	# calculate the geometric mean
	geometricmean <- prod(positivex)**(1/numberofpositives)
	
	return(geometricmean)	
}

# the function takes a vector x. Then it calculates and returns the arithmetic mean of the negative values in the vector x
neg.mean <- function(x){
	
	# select negative numbers and ignore positive numbers
	negativex <- x[(x<0)]
	
	# get the number of negative values in the vector
	numberofnegatives <- length(negativex)
	
	# calculate the arithmetic mean
	arithmeticmean <- sum(negativex)/numberofnegatives
	
	return(arithmeticmean)
}

# the function  takes x, a vector of numeric value. The function will calculate and return the harmonic mean of x
harmonicMean <- function(x){
	
	# select values that are non-zero
	nonzerox <- x[(x!=0)]
	
	# get the number of non-zero values in the vector
	numberofnonzero <- length(nonzerox)
	
	# calculate the harmonic mean of the vector
	harmonicmean <- (sum(1/nonzerox)/numberofnonzero)**-1
	
	return (harmonicmean)
	
}
