## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(data.table)

## Read activity labels, features, training set, training labels, training subject, 
## test set, test labels

activityLabels <- data.table(read.table("activity_labels.txt", as.is = TRUE));
features <- data.table(read.table("features.txt", as.is = TRUE));
trainingSet <- data.table(read.table("train/X_train.txt", as.is = TRUE));
trainingLabels <- data.table(read.table("train/y_train.txt", as.is = TRUE));
trainingSubjects <- data.table(read.table("train/subject_train.txt", as.is = TRUE));
testSet <- data.table(read.table("test/X_test.txt", as.is = TRUE));
testLabels <- data.table(read.table("test/y_test.txt", as.is = TRUE));
testSubjects <- data.table(read.table("test/subject_test.txt", as.is = TRUE));

## Clean the variable names
cleanFeatures <- features$V2
cleanFeatures <- gsub("()", "", cleanFeatures, fixed = TRUE)
cleanFeatures <- gsub("-", ".", cleanFeatures, fixed = TRUE)
cleanFeatures <- gsub("tBody", "TimeBody", cleanFeatures, fixed = TRUE)
cleanFeatures <- gsub("fBody", "FFTBody", cleanFeatures, fixed = TRUE)
cleanFeatures <- gsub("tGravity", "TimeGravity", cleanFeatures, fixed = TRUE)
cleanFeatures <- gsub("fGravity", "FFTGravity", cleanFeatures, fixed = TRUE)

## Set column names to test and training data set
setnames(testSet, cleanFeatures)
setnames(trainingSet, cleanFeatures)

## Uses descriptive activity names to name the activities in the data set
testActivity <- activityLabels$V2[testLabels$V1]
trainingActivity <- activityLabels$V2[trainingLabels$V1]

## Add Subject Id and Activity Name to test and training data set
test <- cbind(testSubjects$V1, testActivity, testSet)
colnames(test)[1] <- "subjectId"
colnames(test)[2] <- "activity"

training <- cbind(trainingSubjects$V1, trainingActivity, trainingSet)
colnames(training)[1] <- "subjectId"
colnames(training)[2] <- "activity"

## Merge test and training data
mergedData <- rbind(test, training)

## Get required features with mean and standard deviation
reqFeatures <- grep("mean|std", colnames(mergedData))

##  Extracts only the measurements on the mean and standard deviation for each measurement
data <- mergedData[, c(1, 2, reqFeatures), with = FALSE]

library(reshape2)

## Melt data to reshape the table
meltedData <- melt(data, id = c("subjectId", "activity"))

## Apply mean to melted data
meanData <- dcast(meltedData, subjectId+activity~..., mean)

## Add avg to column names
finalNames <- names(meanData)
finalNames <- c(finalNames[1:2], paste("avg", finalNames[3:length(finalNames)], sep = ""))
names(meanData) <- finalNames

## Write table to tidy.txt
write.table(meanData, file = "tidy.txt")