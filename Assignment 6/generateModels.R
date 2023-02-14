# generateModels.R
# Author: Yuchen Zhou
# The program will explore whether the population of towns and cities in Canada follow Benford's Law

# load the utility file
source("Utilities.R")

# get the command line arguemnt
args <- commandArgs(trailingOnly=TRUE)

# check whether the number of command line argument is valid
if(length(args)!=2){
	cat ("The script requires two arguments: 1,2 or 3 and a year between 2017 and 2021.\n")
	quit()
}

# check whether the first command line argument is 1,2 or 3
if (!(args[1] %in% c(1,2,3))){
	cat ("please enter 1,2, or 3 as your first argument. Only these options are available.\n")
	quit()
}

# check whether the second command line argument is a year between 2017 and 2021
if((! args[2] %in% c(2017,2018,2019,2020,2021))){
	cat ("Error: the year you entered is not correct. Only the year between 2017 and 2021 should be entered.\n")
	quit()
}

# if the option is 1. The program will use a quadratic model to fit the data
if(args[1]==1){
	mydata=load.data("1710014201-eng.csv")
	digitfreq=build.digit.freqs(mydata,args[2])
	quadratic.fit(digitfreq)

# else if the option is 2. The program will run a generalized linear model to fit the data
} else if (args[1]==2){
	mydata=load.data("1710014201-eng.csv")
	digitfreq=build.digit.freqs(mydata,args[2])
	generalizedlm.fit(digitfreq)

# else if the option is 3. The program will peform chi-squared goodness-of-fit test on the data
}else{
	mydata=load.data("1710014201-eng.csv")
	digitfreq=build.digit.freqs(mydata,args[2])
	chisquared.fit(digitfreq)
}


