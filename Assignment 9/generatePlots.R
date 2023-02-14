# generatePlots.R
# Author: Yuchen Zhou
# This script is the driver script that will call the functions in plottingTools.R to make the two plots

# load the script that contains all the functions
source("plottingTools.R")

# receive two arguments from the command line 
args <- commandArgs(trailingOnly=TRUE)

# check whether the number of command line arguments is valid
if(length(args)!=2){
	cat ("The script requires two arguments. The first argument indicates the file name that will be loaded.\n")
	cat( "The second argument indicates the action it will peform. If plot1 is entered, the script will draw boxplots. If plot2 is entered, the script will draw scatter plot with line of best fit.\n")
	quit()
}

# first argument is the file name. Second argument is the action.
filename=args[1]
action=args[2]

# check whether the first command line argument is a string or a number
if(!is.na(as.numeric(filename))){
	cat("The first argument needs to be a string which is the file name.\n")
	quit()
}

# check whether the second command line argument is plot1 or plot2
if((action!="plot1")&&(action!="plot2")){
	cat("The second argument needs to be either plot1 or plot2. If plot1 is entered, the script will draw boxplots. If plot2 is entered, the script will draw scatter plot with line of best fit.\n")
	quit()
}


# load data
mydata <- load.data(filename)

# if the second command line arugment is plot1, it will call box.plot to generate box plot.
# if the second command line argument is plot2, it will call scatter.plot to generate scatter plot.
if(action=="plot1"){
	box.plot(mydata)
	cat("box plot is generated and saved as plot1.tiff\n")
}else if(action=="plot2"){
	scatter.plot(mydata)
	cat("scatter plot is generated and saved as plot2.tiff\n")
}
