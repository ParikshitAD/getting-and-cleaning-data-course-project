getwd()
setwd('/home/parikshit/Desktop/R')
install.packages("dplyr")
library(dplyr)

# downloading data
FileLocation <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File <- "UCI HAR Dataset.zip"

if (!file.exists(File)) {
        download.file(FileLocation, File, mode = "wb")
}

# unziping zip 
dataLocation <- "UCI HAR Dataset"
if (!file.exists(dataLocation)) {
        unzip(File)
}



# read training data
trainingSubjects <- read.table(file.path(dataLocation, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataLocation, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataLocation, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataLocation, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataLocation, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataLocation, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataLocation, "features.txt"), as.is = TRUE)


# read activity labels
activities <- read.table(file.path(dataLocation, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")



# Merge train and test datasets
humanActivity <- rbind(
        cbind(trainingSubjects, trainingValues, trainingActivity),
        cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")




# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]




# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])



# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols




# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)