# DriverScript.R
# author: Yuchen Zhou
# This script will use the function in Utilities.R and perform analysis on data

source("Utilities.R")

# receive an argument (file name) from the command line and read the file 
filename <- commandArgs(trailingOnly=TRUE)

# check whether the number of command line argument is valid
if(length(filename)!=1){
	cat ("The script requires one argument, indicating the file name that will be loaded.\n")
	quit()
}

# check whether the command line argument is a number not a string
if(!is.na(as.numeric(filename))){
	cat("The argument needs to be a string which is the file name.\n")
	quit()
}

# load data
mydata <- load.data(filename)

# do multiple linear regression on the data
cat ("perform multiple linear regression to predict probability of getting esophageal cancer.\n")
cat("---------------------------------------------------------------------------\n")
multiple.linear.regression(mydata)
cat("---------------------------------------------------------------------------\n\n\n\n")


# perform polynomial regression on data
cat("perform polynomial regression to predict probability of getting esophageal cancer.\n")
cat("---------------------------------------------------------------------------\n")
poly.regression(mydata)
cat("---------------------------------------------------------------------------\n\n\n\n")

# use decision tree to do classification
cat("use desicison tree to classify a patient is at low risk or high risk of getting esophageal cancer.\n")
cat("---------------------------------------------------------------------------\n")
decision.tree(mydata)
cat("---------------------------------------------------------------------------\n\n\n\n")

# use logistic regression to do classification
cat("use logistic regression to classify a patient is at low risk or high risk of getting esophageal cancer.\n")
cat("---------------------------------------------------------------------------\n")
log.regression(mydata)
cat("---------------------------------------------------------------------------\n\n\n\n")

cat("analysis is complete.\n")