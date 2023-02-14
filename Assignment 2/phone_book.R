# Name: Yuchen Zhou
# SciNet username: tmp_yzhou
# Description:
#	script prints phone book contacts filtered out by the specified province

#define vector
names <- c("Alice", "John", "Mike", "Erik", "Frank", "Charlotte")
phone.numbers <- c("416-123-4567", "647-254-3647", "519-254-6534", "416-864-3425", "416-463-3425", "514-635-2462")
cities <- c("Toronto", "Mississauga", "Waterloo", "Toronto", "Oakville", "Montreal")
provinces <- c("ON", "ON", "ON", "ON", "ON", "QC")    

# use vectors to create a data frame and rename the columns in data frame
phone.book <- data.frame(names,phone.numbers,cities,provinces)
colnames(phone.book) <- c("Name","Phone","City","Province")
search.province <- "ON"

# use conditional splicing to find which contacts live in Ontario
cat("ON contacts:\n")
cat("---------------------------------------------------------\n")
province.contacts <- phone.book[phone.book$"Province" == search.province,]
print(province.contacts)
cat("---------------------------------------------------------\n")

# use nrow to find contacts who live outside of GTA
GTA <- c("Toronto","Oakville","Mississauga")
city.notGTA <- nrow(phone.book[!(phone.book$City %in% GTA),])
cat("Total number of contacts living outside of the Greater Toronto Area is ",city.notGTA,"\n")





