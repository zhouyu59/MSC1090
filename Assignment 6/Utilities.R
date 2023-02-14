# Utilities.R
# Author: Yuchen Zhou
# The script contains all functions that will be used to perform tasks in generateModels.R

# The function loads the data with a specific file name and return the data frame 
load.data <-function(filename){
	mydata <- read.csv(filename)
	return(mydata)
}

# The function takes a data frame and a year as arguments. Then it returns a data frame that contains the frequencies of all possible first digits
build.digit.freqs <- function(mydataframe,year){
	
	# get the data from the specific year
	yeardata <- mydataframe[,paste("X",year,sep="")]
	
	# exclude data that have a popualtion of 0 or ..
	selectivedata <-yeardata[(yeardata!=0)&(yeardata!="..")]
	
	# select the first digit of the population
	selectivedatacharacter <- as.character(selectivedata)
	firstdigitdata <-substr(selectivedatacharacter,1,1)
	
	# determine the freuqnecy and return the frequency in a data frame
	firstdigitfrequency<-table(firstdigitdata)
	firstdigitfrequencydf <- as.data.frame(firstdigitfrequency)
	colnames(firstdigitfrequencydf) <- c("digits","counts")
	firstdigitfrequencydf$digits <- as.numeric(firstdigitfrequencydf$digits)
	return (firstdigitfrequencydf)
	
}

# The function takes a data frame. Then it implements a quadratic model which fits the data and provides the details of the model. It also generates a graphical representation of the model
quadratic.fit <- function(my.data){
	
	cat("Fitting a quadratic model.\n")
	
	# fit the data using quadratic model
	model2 <- lm(counts ~ poly(digits,2), data=my.data)
	
	# output the details of the model
	print(summary(model2))
	
	# plot the model with the original data
	plot(my.data$digits,my.data$counts)
	xvalue <- seq(min(my.data$digits),max(my.data$digits),len=100)
	yvalue <- predict(model2, data.frame(digits=xvalue))
	lines(xvalue,yvalue,lwd=2,col="red")
		
}

# The function takes a data frame. Then it implements a generalized linear model which fits the data and provides the details of the model. It also generates a graphical representation of the model
generalizedlm.fit <- function(my.data){

	
	cat ("Fitting a generalized linear model.\n")
	
	# fit the data using generalized linear model with poisson noise model and log link function
	generalizedlm <- glm(counts ~ digits, data=my.data, family=poisson(link=log))
	
	# output the details of the model
	print (summary(generalizedlm))
	
	# plot the model with the original data
	plot(my.data$digits,my.data$counts)
	lines(my.data$digits,predict(generalizedlm,data.frame(digits=my.data$digits),type="response"), col="red")
}

# The function takes a data frame. Then it performs a chi-squared goodness-of-fit test on the data to see if the data are statistically different from the distribution associated with Benford's law
chisquared.fit <- function(my.data){
	
	cat("Performing a chi-squared goodness-of-fit test.\n")
	
	# calculate the probability distribution for Benford's law
	firstdigit=c(1,2,3,4,5,6,7,8,9)
	probabilityfirstdigit=log10((firstdigit+c(1,1,1,1,1,1,1,1,1))/firstdigit)
	
	# do chi-squared goodness-of-fit test
	testresult <- chisq.test(my.data$counts,p=probabilityfirstdigit)
	
	# output the results
	if(testresult$p.value<0.05){
		cat("The null hypothesis that the data follow Benford's law is rejected, with a p value of",testresult$p.value,".\n")
	}else{
		cat("The null hypothesis that the data follow Benford's law is not rejected, with a p value of",testresult$p.value,".\n")
	}
	
	return(testresult)
	
	
	
}
