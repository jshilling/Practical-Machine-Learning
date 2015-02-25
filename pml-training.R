setwd("./Practical Machine Learning")

### download the data set ###
getFile <- function (targetFile,targetURL) 
        {if (!file.exists(targetFile)) {
                if (!file.exists(targetURL)) {
                        download.file(url=targetURL, destfile=targetFile,
                                      method="auto", quiet=TRUE, cacheOK=FALSE)
                }
                dateDownloaded <- date()
                histName <- paste(targetFile, "_History.txt", sep="")
                what <- paste("File: ", targetFile)
                where <- paste("From: ", targetURL)
                when <-paste("  On: ", dateDownloaded)
                write(paste(what, where, when, sep="\n"), histName)
                rm(histName, what, where, when)
        }}

### training data
file <- "pml-training.csv"
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
getFile(file,URL)
originalData <- read.csv(file, header = T, fill = TRUE, stringsAsFactors=FALSE)
### testing data
file <- "pml-testing.csv"
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
getFile(file,URL)
testing <- read.csv(file, header = T, fill = TRUE, stringsAsFactors=FALSE)
### end download ###

### libraries
library(caret)
library(randomForest)

## Data Cleaning
dim(originalData)
working <- originalData
### remove summary rows and make the response variable a factor
working <- working[working$new_window=="no",]
working$classe <- as.factor(working$classe) 
### drop the non-predictive labelling variables
working <- working[,-c(1:6)]
### remove variables that won't work with random forest
### sepcifically, variable that are characters because of missing data
keep <- !(sapply(working, is.character))
working <- working[,keep]
missing <- sapply(working, function(x) sum(is.na(x)) > 0)
working <- working[,!missing]
### remove variables with near zero variance
NZV <- nearZeroVar(working[,-1], saveMetrics=TRUE)$nzv
working <- working[,!NZV]

### split the working data into training and validation sets
set.seed(6789)
inTrain <- createDataPartition(y=working$classe , p=0.8, list=FALSE)
training <- working[inTrain,]
validation <- working[-inTrain,]

fit <- randomForest(classe ~ ., data=training)
print(fit, digits=3)

predictions <- predict(fit, validation)
confusionMatrix(predictions, validation$classe)

qplot(predictions,classe,data=validation)

outOfSample <- data.frame(Accuracy = sum(predictions == validation$classe)/length(predictions))
outOfSample$Error <- 1 - outOfSample$Accuracy
print(outOfSample, digits=3)


testPredictions <- as.character(predict(fit, testing))



# write up
pml_write_files = function(x) {
        n = length(x)
        for (i in 1:n) {
                filename = paste0("problem_id_", i, ".txt")
                write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                            col.names = FALSE)
        }
}

pml_write_files(testPredictions)


