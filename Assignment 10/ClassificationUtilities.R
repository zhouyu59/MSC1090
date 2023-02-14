# ClassificationUtilities.R
# Author: Yuchen Zhou
# this script will contain all utility functions for classification 

# load the libraries that are required for the utility functions
library(caret)
library(english)

# this function takes a filename or URL and load the data in the file into a dataframe. Then the function will create a stratified
# split of the data into training and testing data sets
load.stratify.data <- function(filename){
	
	# read the data fro the file
	mydata <- read.table(filename,,header=TRUE,sep=',',dec=".")
	
	# make the label name valid for knn algorithm

	if((!is.na(as.numeric(mydata$label[1])))&&(!is.factor(mydata$label[1]))){

		mydata$label <- as.character(english(mydata$label))
	}
	
			
	mydata$label <- as.factor(mydata$label)
	
	
	# create stratified split
	index <- createDataPartition(mydata$label,p=0.75,list=FALSE)
	trainSet <- mydata[index,]
	testSet <- mydata[-index,]
	my_list <- list(trainSet,testSet)
	return(my_list)
	
}

# this function takes 2 arguments. The first argument is the training data. The second argument is the type of classification model.
# the function will examine the second argument and create a model for training data with the given classification model type
classification.model <- function(trainingData,modelType){
	
	# if the model type is DT, the function will return the decision tree model for the training data
	if(modelType=="DT"){
		
		tree.model = train (label ~ .,data=trainingData,method="rpart")
		return (tree.model)
	
	# if the model type is SVM, the function will return the SVM model for the training data	
	}else if (modelType =="SVM"){
		
		SVM.model = train(label ~.,data=trainingData,method="svmLinear",preProc=c('center','scale'))
		return(SVM.model)
		
	}else{
		cat("The classification model type is not valid.\n")
	}
}

# this function takes 1 argument which is the training data. The function will perform 10-fold cross-validation on a kNN model. The 
# function will return the final kNN model
kNN.cvmodel <- function(trainingData){
	
	fitControl <- trainControl(method="cv",number=10,classProbs=TRUE)
	
	knnmodel <- train(label ~., data=trainingData, method="knn",trControl=fitControl,preProc=c('center','scale'))
	
	return(knnmodel)
}

# this function takes 2 arguments, a pre-trained model and the test data set. The function prints out thee confusion matrix and the model accuracy
# based on the test data
model.test.performance <- function(myModel, testData){
	
	# use pre-trained model to predict test set
	predictResult <- predict(myModel, newdata=testData)
	
	# build the confusion matrix
	myconfusionmatrix <- confusionMatrix(predictResult,testData$label)
	
	cat("Confusion matrix:\n")
	print(myconfusionmatrix$table)
	
	# output the accuracy of the model
	cat("Accuracy:",(myconfusionmatrix$overall)[1],"\n")
}