###########################################
##  Getting and Cleaning Data - Project  ##
###########################################

#  You should create one R script called run_analysis.R that does the following. 
#   1) Merges the training and the test sets to create one data set.
#   2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#   3) Uses descriptive activity names to name the activities in the data set
#   4) Appropriately labels the data set with descriptive activity names. 
#   5) Creates a second, independent tidy data set with the average of each variable 
#      for each activity and each subject.

# Set WD for personal Use #
#setwd("C:/Users/Alex/Dropbox/Training/Coursera/3 Getting and Cleaning Data/Project")

## Load Data Labels and headings ##
labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
colnames(labels) <- c("activityid","activity")
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

## Remove punctuation from string ##
features$V2 <- gsub("[[:punct:]]","",features$V2)


## Load Train and Test Data ##
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")
colnames(ytrain) <- c("activityid")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
ytest <- read.table("UCI HAR Dataset/test/y_test.txt")
colnames(ytest) <- c("activityid")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Join x Files
xfiles <-rbind(xtest,xtrain)
# Join y Files
yfiles <-rbind(ytest,ytrain)
# Join Subject Files
subjectfiles <- rbind(subjectTest,subjectTrain)
colnames(sujectfiles) <- c("subject")
#get the entries from features in as the headings for x
colnames(xfiles) <- features[,2]
# combine the joined x, the joined y, and the joined subject
data <- cbind(subjectfiles,yfiles,xfiles)
# get the activity labels in based on the y column (one way is merge)
data <- merge(labels,data, by.x="activityid", by.y="activityid", all=TRUE)

#Remove previous datasets to save memory#
#remove(features)
remove(labels)
remove(subjectTest)
remove(subjectTrain)
remove(subjectfiles)
remove(xtest)
remove(xtrain)
remove(xfiles)
remove(ytest)
remove(ytrain)
remove(yfiles)

##   2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#set needed variables to vars and extract only those columns from the dataset
vars <- !grepl("Freq", features$V2) & grepl("mean", features$V2) | grepl("std", features$V2)
vars <- append(c(TRUE, TRUE, TRUE), vars)
remove(features)

# 5) Creates a second, independent tidy data set with the average of each variable 
#      for each activity and each subject.
tidyDataset <- data[,vars]
tidyDataset <- aggregate(tidyDataset, by=list("subject"=tidyDataset$subject,
            "activity"=tidyDataset$activity, "activityid"=tidyDataset$activityid), FUN = "mean")
tidyDataset <- tidyDataset[,-c(3:6)]


######################################
### Create Output files for Course ###

# Create Tidy Dataset output file
#write.csv(tidyDataset,"tidyDataset.csv")
write.table(tidyDataset, "tidyDataset.txt")

# Create table with variables that you can just paste into your markdown CodeBook.md file
listOfVariables <- data.frame(names(tidyDataset))
write.csv(listOfVariables,"listOfVariables.csv")
