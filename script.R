###############################
# Where to start the analysis #
# #############################
# Authors:  Susan Brake       #
#           Matthew Odom      #
#           Kishen Jayanti    #
###############################
rm(list=ls())
# install.packages("Hmisc"); 
# install.packages("gmodels");
# install.packages("reshape");
library("Hmisc")
library("gmodels") 
library("reshape")

# import information from Watson
Orig.data <- read.csv("train.csv", header = T)
data <- Orig.data

# Data creation/manipulation
#########################################

data$Open.Date <- as.Date(data$Open.Date, format = "%m/%d/%Y")
# data$year <- format(data$Open.Date, format = "%Y")
# data$month <- format(data$Open.Date, format = "%m")

# Data Exploration
#########################################
test <- data.frame(sapply(data [sapply(data, is.integer)==T], as.numeric))


# Crosstabs
##########################################
vars <- names(data) [c(-1,-2)]; vars <- vars[c(-41)]
crosstabs <- data.frame()

for(i in 1:length(vars)){ # numeric
  iter <- data.frame(cast(as.data.frame(CrossTable(cut2(data [,which(colnames(data)==vars[4])],g=11),data$revenue)$t),x~y))
  crosstabs <- rbind(crosstabs, data.frame(iter))
}

# For every unique value in City make a dummy variable
# ~~~same for city group
for(level in unique(data$City)){
  data[paste("dummy",level,sep = "_")] <- ifelse(data$City == level, 1, 0)
}

for(level in unique(data$City.Group)){
  data[paste("dummy",level,sep = "_")] <- ifelse(data$City.Group == level, 1, 0)
}

for(level in unique(data$Type)){
  data[paste("dummy",level,sep = "_")] <- ifelse(data$Type == level, 1, 0)
}

copy_data <- data

# First regression
data <- data [,c(-1,-2,-3,-4,-5)]
lm1 <- lm(revenue~.,data=data)
summary_lm1 <- summary(lm1)




