# processTTC.R
# Author: Yuchen Zhou
# The program outputs information about TTC streetcar delays

# receive an argument (file name) from the command line and read the file 
filename <- commandArgs(trailingOnly=TRUE)
cat("Processing data from file:",filename,"\n")

# put the file's data into a dataframe
data <- read.csv(filename)

# automatically identify the different types of incidents
incidentType <- unique(data$Incident)

# compute total number for each incident type
cat("Total number of delays per incident type:\n")
for (oneincident in incidentType){
	numberofincident<-sum(data$Incident==oneincident)
	cat(oneincident," -- ",numberofincident,"\n")
}

# calculate and print the average minimum delay of streetcars due to a mechanical incident
mechanicaldelay<-data[data$Incident=="Mechanical",]
averagemindelay<-mean(mechanicaldelay$Min.Delay,na.rm=TRUE)
cat("The average minimum delay of the streetcars due to a mechanical incident,\n")
cat("ignoring unreported data, is ",averagemindelay," minutes.\n")

# determine the route with the most delays in February
# get february data
februarydata<-data[substr(data$Report.Date,6,7)=="02",]
# get the frequency for february data
delayfrequency<-table(februarydata$Route)
# order the frequency in descending order and get the one with the most delay
nameinorder<-names(sort(delayfrequency,decreasing=TRUE))
cat("The route with the msot delays in February was route",nameinorder[1],"\n")