rm(list=ls())

library(RCurl)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# set working directory to the Github folder
setwd("~/Github/practical-machine-learning")

# check to see if the CSV files are in a data directory and if not download them
if (!file.exists("./Data/training.csv")) {
        training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(training_url, destfile = "./Data/training.csv", method = "libcurl")
}

if (!file.exists("./Data/test.csv")) {
        test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(test_url, destfile = "./Data/test.csv", method = "libcurl")
        
}

# build training dataset from attributes that contain belt, forearm, arm, and dumbell

training <- read.csv("./Data/training.csv", header = TRUE, na.strings=c("NA","NaN", " "))
# Use a regex to find features that are of interest
# Add user name back
training_cols <- grepl("arm|belt|dumbell|classe", names(training), ignore.case = TRUE)
# Remove columns not of interest
training <- training[training_cols]
colnames_train <- colnames(training)

# get test dataset
testing <- read.csv("./Data/test.csv", header = TRUE, na.strings=c("NA","NaN", " "))
testing_cols <- grepl("arm|belt|dumbell|classe", names(training), ignore.case = TRUE)
testing <- testing[training_cols]

# remove columns that are blank or DIV/0!
# Count the number of non-NAs in each col.
nonNAs <- function(x) {
        as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# Build vector of missing data or NA columns to drop.
colcnts <- nonNAs(training)
drops <- c()
for (cnt in 1:length(colcnts)) {
        if (colcnts[cnt] < nrow(training)) {
                drops <- c(drops, colnames_train[cnt])
        }
}

# Drop NA data
training <- training[,!(names(training) %in% drops)]
training <- training[,1:length(colnames(training))]

testing <- testing[,!(names(testing) %in% drops)]
testing <- testing[,1:length(colnames(testing))]

# Show remaining columns.
colnames(training)

# Confirm attributes between training and test are identical
colnames_train <- names(training)
colnames_test <- names(testing)
all.equal(colnames_train[1:length(colnames_train)-1], colnames_test[1:length(colnames_train)-1])

# Need to check for nzv and blank columns


# Partition and Clean Data Set

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; 
myTraining2 <- training[-inTrain, ]
dim(myTraining); dim(myTraining2)

PP_myTraining <- preProcess(myTraining[,-64], method = c("center", "scale", "nzv", "pca"))
PP_myTraining

predict_training <- predict(PP_myTraining[,-64], myTraining2)

