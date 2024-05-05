setwd("/Users/cuiying/Desktop/R studio")

data <- read.csv("P3-Future-500-The-Dataset.csv", stringsAsFactors = TRUE)
summary(data)
str(data)

#FVT
typeof(data$Revenue)
data$Revenue <- as.numeric(data$Revenue)

data$Expenses <- gsub("Dollars","",data$Expenses)
data$Expenses <- gsub(",","",data$Expenses)
data$Revenue <- gsub("\\$","",data$Revenue)
data$Revenue <- gsub(",","",data$Revenue)
data$Growth <- gsub("%","",data$Growth)
str(data)

data$Expenses <- as.numeric(data$Expenses)
data$Revenue <- as.numeric(data$Revenue)
data$Growth <- as.numeric(data$Growth)
str(data)

#Missing value
data[complete.cases(data),]
data <- read.csv("P3-Future-500-The-Dataset.csv", na.strings=c(""), stringsAsFactors = TRUE)
head(data,20)
str(data)


is.na(data$Expenses)
data[is.na(data$Expenses),]
data[is.na(data$State),]
data[is.na(data$Employees),]

data.backup <- data
data[!complete.cases(data),]
data[is.na(data$Industry),]
data <- data[!is.na(data$Industry),]

rownames(data) <- 1:nrow(data)
rownames(data) <- NULL
data
tail(data)

#replace missing value
data[!complete.cases(data),]
data[is.na(data$State),]

data[is.na(data$State) & data$City=="New York",]
data[is.na(data$State) & data$City=="New York","State"] <- "NY"

data[c(11,377),]

data[is.na(data$State) & data$City=="San Francisco","State"] <- "CA"
data[c(82,265),]

data[!complete.cases(data),]
median(data[,"Employees"],na.rm=T) #na.rm: to exclude any missing values from the calculation
median(data[data$Industry=="Retail","Employees"],na.rm=T)
med_empl_retail <- median(data[data$Industry=="Retail","Employees"],na.rm=T)
data[is.na(data$Employees) & data$Industry=="Retail","Employees"] <- med_empl_retail
data[3,]

med_empl_fs <- median(data[data$Industry=="Financial Services","Employees"],na.rm=T)
data[is.na(data$Employees) & data$Industry=="Financial Services","Employees"] <- med_empl_fs
data[330,]

data[!complete.cases(data),]
med_growth_constr <- median(data[data$Industry=="Construction","Growth"],na.rm=T)
data[is.na(data$Growth) & data$Industry=="Construction","Growth"] <- med_growth_constr
data[8,]

data[!complete.cases(data),]
med_rev_constr <- median(data[data$Industry=="Construction","Revenue"],na.rm=T)
data[is.na(data$Revenue) & data$Industry=="Construction","Revenue"] <- med_rev_constr
data[8,]

med_rev_constr <- median(data[data$Industry=="Construction","Revenue"],na.rm=T)
data[is.na(data$Revenue) & data$Industry=="Construction","Revenue"] <- med_rev_constr
data[8,]
data[42,]

med_exp_constr <- median(data[data$Industry=="Construction","Expenses"],na.rm=T)
med_exp_it <- median(data[data$Industry=="Construction","Expenses"],na.rm=T)

data[is.na(data$Expenses) & data$Industry=="Construction" & is.na(data$Profit),]
data[is.na(data$Expenses) & data$Industry=="Construction" & is.na(data$Profit),"Expenses"] <- med_exp_constr

data[is.na(data$Expenses) & data$Industry=="IT Services" & is.na(data$Profit),"Expenses"] <- med_exp_it
data[!complete.cases(data),]

data[is.na(data$Profit),"Profit"] <- data[is.na(data$Profit),"Revenue"] - data[is.na(data$Profit),"Expenses"]
data[c(8,42),]
str(data)
data$Profit <- as.numeric(as.character(data$Profit))

data[is.na(data$Expenses),"Expenses"] <- data[is.na(data$Expenses),"Revenue"] - data[is.na(data$Expenses),"Profit"]
data[15,]

#visualizing the results
install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data)
p + geom_point(aes(x=Revenue, y=Expenses, colour=Industry, size=Profit))

d <- ggplot(data=data, aes(x=Revenue, y=Expenses, colour=Industry))
d + geom_point() + geom_smooth(fill=NA, size=1.2)

f <- ggplot(data=data, aes(x=Industry, y=Growth, colour=Industry))
f + geom_boxplot()
f + geom_jitter() + geom_boxplot(size=0.5, alpha=0.5, outlier.colour=NA)

