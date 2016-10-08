run_analysis <- function(){
  mergeSubject <- function(dataSet, subject){
    cbind(dataSet, subject)
  }
  
  mergeActivity <- function(dataSet, activity){
    cbind(dataSet, activity)
  }
  
  mergeTrainTestSet <- function(train, test){
    rbind(train, test)
  }
  
  getMeanStdCol <- function(dataSet){
    dataSet[grep("mean\\b|std\\b",features$V2),1:2]
  }
  
  ## 
  ## Part 1a - get train data
  trainingSet <- read.table("./train/X_train.txt")
  subject <- read.table("./train/subject_train.txt")
  activity <-  read.table("./train/y_train.txt")
  mergedTrainingSet <- mergeActivity(mergeSubject(trainingSet, subject), activity)
  
  ## Part 1b - get test data
  testingSet <- read.table("./test/X_test.txt")
  subject <- read.table("./test/subject_test.txt")
  activity <-  read.table("./test/y_test.txt")
  mergedTestingSet <- mergeActivity(mergeSubject(testingSet, subject), activity)
  
  ## Part 1c - merge train and test data and create one set
  mergedDataSet <- mergeTrainTestSet(mergedTrainingSet, mergedTestingSet)
  
  ## 
  ## Part 2a
  features <- read.table("features.txt")
  meanStdIndex <- getMeanStdCol(features)
  
  ## Part 2b
  includedCol <- c(meanStdIndex[,1], ncol(mergedDataSet)-1, ncol(mergedDataSet))
  
  ## Part 2c
  filteredDataSet <- mergedDataSet[,includedCol]
  
  ## Part 3
  filteredDataSet <- setNames(filteredDataSet,c(as.character(meanStdIndex[,2]), "subject", "activity_names"))
  
  ## 
  ## Part 4a - read labels 
  activityLabels <-read.table("activity_labels.txt")
  activityLabels[,2] <- as.character(activityLabels[,2])
  
  ## Part 4b; assign labels 
  tidyDataSet <- filteredDataSet
  tidyDataSet$activity_names <- activityLabels[match(tidyDataSet$activity_names,activityLabels[,1]),2]

  ## Part 5 - tiday dataset with mean 
  endCol <- ncol(tidyDataSet)-2
  newTidySet <- aggregate(as.matrix(tidyDataSet[,1:endCol])~ subject + activity_names, tidyDataSet, mean)
  newTidySet
}

# to test the Function - run this:
# head(run_analysis())
