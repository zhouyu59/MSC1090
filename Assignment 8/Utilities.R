# Utilities.R
# Author: Yuchen Zhou
# The script contasins all functions that will be used to form statistical analysis on the data


# load the library that is required for performing analysis
library (rpart)
library(rpart.plot)
library(ROCR)

# The function loads the data with a specific file name and return the data frame
load.data <- function(filename){
	mydata <- read.csv(filename)
	return(mydata)
}

# the function performs multiple linear regression on the data to predict probability of getting cancer
multiple.linear.regression <- function(mydataframe){
	
	# the independent variable is averageAge, alcgp, tobgp
	model <- lm(probability ~ averageAge+alcgp+tobgp,data=mydataframe)
	print(summary(model))
	myRSE <- sigma(model)
	cat("the residual standard error of the model is",myRSE,".\n")
	
	# create the plots to check the fit of the model
	par(mfrow=c(1,5))
	plot(model$residuals)
	plot(mydataframe$averageAge,model$residuals)
	plot(mydataframe$alcgp,model$residuals)
	plot(mydataframe$tobgp,model$residuals)
	plot(mydataframe$probability,model$residuals)
	
	par(mfrow=c(1,1))
	hist(model$residuals, breaks=11)
	
	qqnorm(model$residuals)
	qqline(model$residuals)
}

# the function performs polynomial regression on the data to predict probability of getting cancer
poly.regression <- function(mydataframe){
	model <- lm(probability ~ poly(averageAge,5)+poly(alcgp,3)+poly(tobgp,3)+alcgp:tobgp+averageAge:alcgp+averageAge:tobgp+0,data=mydataframe)
	print(summary(model))
	
	# create the plots to check the fit of the model
	par(mfrow=c(1,5))
	plot(model$residuals)
	plot(mydataframe$averageAge,model$residuals)
	plot(mydataframe$alcgp,model$residuals)
	plot(mydataframe$tobgp,model$residuals)
	plot(mydataframe$probability,model$residuals)
	
	par(mfrow=c(1,1))
	hist(model$residuals, breaks=11)
	qqnorm(model$residuals)
	qqline(model$residuals)
}

# the function uses decision tree to classify whether the patients have high or low risk of getting cancer
decision.tree <- function(mydataframe){
	
	# split the data into training set and test set
	ind <- sample(c(T,F),nrow(mydataframe),replace=T,prob=c(0.7,0.3))
	train.data <- mydataframe[ind,]
	test.data <- mydataframe[!ind,]
	
	# build the decision tree using training set
	f <- lowrisk ~ averageAge+alcgp+tobgp
	mydataframe.tree <- rpart(f,data=train.data,method="class")
	rpart.plot(mydataframe.tree)
	
	# test the decision tree's accuracy using trainining set
	pred <- predict(mydataframe.tree, type="class")
	trainaccuracy=sum(train.data$lowrisk==pred)/nrow(train.data)
	cat("The accuracy of decision tree for classifying training data is",trainaccuracy,".\n")
	print(table(train.data$lowrisk,pred))
	
	# test the decision tree's accuracy using test set
	testPred <- predict(mydataframe.tree,newdata=test.data,type="class")
	testaccuracy=sum(test.data$lowrisk==testPred)/nrow(test.data)
	cat("The accuracy of decision tree for classifying testing data is",testaccuracy,".\n")
	print(table(test.data$lowrisk,testPred))
	
}

# the function uses logistic regression to classify whether the patients have high or low risk of getting cancer
log.regression <- function(mydataframe){
	
	# split the data into training set and test set
	ind <- sample(c(T,F),nrow(mydataframe),replace=T,prob=c(0.7,0.3))
	train.data <- mydataframe[ind,]
	test.data <- mydataframe[!ind,]
	
	# build logistic model and check the accuracy of the model using test set
	model <- glm(lowrisk ~ averageAge+alcgp+tobgp,family="binomial",data=train.data)
	print(summary(model))
	pred <- predict(model,newdata=test.data,type="response")
	new.pred <- ifelse(pred>0.5,'TRUE','FALSE')
	conf <- table(test.data$lowrisk,as.factor(new.pred))
	print(conf)
	testaccuracy <- sum(diag(conf))/sum(conf)
	cat("The accuracy of logistic regression model for classifying test data is",testaccuracy,".\n")
	
	# create ROC curve and calculate the AUC of the ROC
	ROC.pred <- prediction(pred,test.data$lowrisk)
	ROC.perf<- performance(ROC.pred,measure='tpr',x.measure='fpr')
	plot(ROC.perf,type='b',pch=21,bg='blue')
	lines(c(0,1),c(0,1),lty=2)
	
	ROC.AUC<- performance(ROC.pred,measure='auc')
	aucvalue<-ROC.AUC@y.values[[1]]
	
	cat("The AUC of ROC curve is",aucvalue,".\n")
}


