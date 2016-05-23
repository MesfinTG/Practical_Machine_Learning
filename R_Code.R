trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(trainUrl, destfile= "./pm1_training.csv")
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(testUrl, destfile= "./pm1_testing.csv")
dateDownloaded <- date()
dateDownloaded

## load datasets in R
train.data <- read.csv("pm1_training.csv", header=TRUE)
test.data <- read.csv("pm1_testing.csv", header=TRUE)
str(train.data)

## estimate the extent of missing values in the datasets
mean(is.na(train.data))

## Create a lookup table for the variables with missing values and subset the raw datasets without these variables

cols_to_drop <- grep("^X|^var_|^stddev_|^avg|^kurtosis_|^skewness_|^min_*|^max_|^amplitude_*|*_timestamp*|*_window*|^user", names(train.data))

train.data <- train.data[-cols_to_drop]
test.data <- test.data[-cols_to_drop]

## partition raw data into 70% testing and 30% testing data set

library(caret)
inTrain <- createDataPartition(train.data$classe, p=0.7, list=FALSE) 

set.seed(324)

training <- train.data[inTrain,]
testing <- train.data[-inTrain,]
rm(train.data)

modFit <- train(classe ~ ., method="rf", data=training)

# save(modFit, file=paste (fileName, ".Rdata", sep=" "))

print(modFit)

## variable importance of predictive model
varImp(modFit)

# Evaluate the predictive model on the testing dataset
prediction <- predict(modFit, newdata=testing)

confusionMatrix(prediction, testing$classe)
