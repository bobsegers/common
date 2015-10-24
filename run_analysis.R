# Assignment:
# You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(data.table)
library(dplyr)

# STEP 0 - Load all data

# Read Supporting Metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Part 1 - Merge the training and the test sets to create one data set

# Merge the training and the test sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming the columns
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

# append columns 
completeData <- cbind(features,activity,subject)

# Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement

# Extract the column indices that have either mean or std in them.
columnsWithMeanStd <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanStd, 562, 563)

# We create requiredData with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns.
requiredData <- completeData[,requiredColumns]

# Part 3 - Uses descriptive activity names to name the activities in the data set

# We need to change the type of the character column content to character so that it can accept activity names. The activity names are taken from metadata activityLabels.
requiredData$Activity <- as.character(requiredData$Activity)
for (i in 1:6){
  requiredData$Activity[requiredData$Activity == i] <- as.character(activityLabels[i,2])
}

# We need to factor the activity variable, once the activity names are updated.
# requiredData$Activity <- as.factor(requiredData$Activity)

# Part 4 - Appropriately labels the data set with descriptive variable names
names(requiredData)<-gsub("Acc", "Accelerometer", names(requiredData))
names(requiredData)<-gsub("Gyro", "Gyroscope", names(requiredData))
names(requiredData)<-gsub("BodyBody", "Body", names(requiredData))
names(requiredData)<-gsub("Mag", "Magnitude", names(requiredData))
names(requiredData)<-gsub("^t", "Time", names(requiredData))
names(requiredData)<-gsub("^f", "Frequency", names(requiredData))
names(requiredData)<-gsub("tBody", "TimeBody", names(requiredData))
names(requiredData)<-gsub("-mean()", "Mean", names(requiredData), ignore.case = TRUE)
names(requiredData)<-gsub("-std()", "STD", names(requiredData), ignore.case = TRUE)
names(requiredData)<-gsub("-freq()", "Frequency", names(requiredData), ignore.case = TRUE)
names(requiredData)<-gsub("angle", "Angle", names(requiredData))
names(requiredData)<-gsub("gravity", "Gravity", names(requiredData))

# Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

# Firstly, let us set Subject as a factor variable.
# requiredData$Subject <- as.factor(requiredData$Subject)
requiredData <- data.table(requiredData)

# We create tidyData as a data set with average for each activity and subject. Then, we order the enties in tidyData and write it into data file Tidy.txt that contains the processed data.
tidyData <- aggregate(. ~Subject + Activity, requiredData, mean)
# tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
