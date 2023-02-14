# ClassificationDriver.R
# Author: Yuchen Zhou
# The script will run the classification algorithms and print out the confusion matrix and accuracy from running the model on the test data

# load the script that contains all the functions
source("ClassificationUtilities.R")

# receive two arguments from the command line 
args <- commandArgs(trailingOnly=TRUE)

# check whether the number of command line arguments is valid
if(length(args)!=2){
	cat ("The script requires two arguments. The first argument indicates the file name or URL that will be loaded.\n")
	cat( "The second argument indicates the type of classification model, either DT, KNN, or SVM. \n")
	quit()
}

# first argument is the file name. Second argument is the model type
filename=args[1]
modeltype=args[2]

# check whether the first command line argument is a string or a number
if(!is.na(as.numeric(filename))){
	cat("The first argument needs to be a string which is the file name or URL.\n")
	quit()
}

# check whether the second command line argument is DT,KNN,or SVM
if((modeltype!="DT")&&(modeltype!="KNN")&&(modeltype!="SVM")){
	cat("The second argument needs to be either DT, KNN, or SVM.\n")
	quit()
}

# load the data from the file or url and split the data
cat("Gathering data from",filename,"\n")
stratifydata <- load.stratify.data(filename)

trainingdata <- stratifydata[[1]]
testdata <- stratifydata[[2]]

# run functions according to the type of the model
if(modeltype=="DT"){
	
	cat("Building Decision Tree model.\n")
	mymodel<-classification.model(trainingdata,modeltype)
	
}else if(modeltype=="SVM"){
	
	cat("Building Support Vector Machines model.\n")
	mymodel<-classification.model(trainingdata,modeltype)
	
}else{
	
	cat("Building kNN model.\n")
	mymodel<-kNN.cvmodel(trainingdata)
	
}

# print out the confusion matrix and accuracy
model.test.performance(mymodel,testdata)