#' ---
#' title: "run_analysis"
#' author: "AO"
#' date: "12/01/2020"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Getting and Cleaning Data Course Project: R Script
#' 
#' You should create one R script called run_analysis.R that does the following. 
#' 1.Merges the training and the test sets to create one data set.
#' 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#' 3.Uses descriptive activity names to name the activities in the data set
#' 4.Appropriately labels the data set with descriptive variable names. 
#' 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#' 
#' ### Please Note: These actions have not been performed in numerical order.
#' The lines of code at which the actions contain a ### prefix, and a suffix with the task number:
#' 1.Merges the training and the test sets to create one data set - From line 136
#' 2.Extracts only the measurements on the mean and standard deviation for each measurement - From line 91
#' 3.Uses descriptive activity names to name the activities in the data set - From line 143 (also 76-83)
#' 4.Appropriately labels the data set with descriptive variable names - From line 163
#' 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject - From line 188
#' 
#' Loading the libraries needed for this code
## ------------------------------------------------------------------------
library(data.table)
library(tidyverse)

#' 
#' Checking if the folder exists
## ------------------------------------------------------------------------
# Checking if the directory exists, creating one if not.
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl") 

# Unzipping the UNI HAR dataset folder.
if (!file.exists("UCI HAR Dataset")) { 
  unzip(zipfile="./data/Dataset.zip",exdir="./data") 
}


#' List the files in the folder
## ------------------------------------------------------------------------
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

#' 
#' 
#' Read in the test and train data files.
## ------------------------------------------------------------------------
#Train data
FeaturesTrainData <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ActivityTrainData <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
SubjectTrainData <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")


#Test data
FeaturesTestData <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ActivityTestData <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
SubjectTestData <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")


#' 
#' Read activity labels table.
## ------------------------------------------------------------------------
ActivityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")[,2]

#' 
#' Set activity labels (point 3)
## ------------------------------------------------------------------------
ActivityTestData[,2] = ActivityLabels[ActivityTestData[,1]]
names(ActivityTestData) = c("activityID", "activity")

ActivityTrainData[,2] = ActivityLabels[ActivityTrainData[,1]]
names(ActivityTrainData) = c("activityID", "activity")

#' 
#' Read features table
#' 
## ------------------------------------------------------------------------
FeaturesTable <- read.table("./data/UCI HAR Dataset/features.txt")[,2]

#' 
#' Keep only means and standard deviations from the Features table (Point 2)
#' 
## ------------------------------------------------------------------------
SubsetNames <-grepl("mean|std", FeaturesTable)

#' 
#' Assign varible names
#' 
## ------------------------------------------------------------------------
names(SubjectTestData) <-c("subject")
names(SubjectTrainData) <-c("subject")
names(FeaturesTestData) = FeaturesTable
names(FeaturesTrainData) = FeaturesTable

#' 
## ------------------------------------------------------------------------
NamesCleaned <- sapply(FeaturesTable, function(x) {gsub("[()]", "",x)})
names(FeaturesTestData) <- NamesCleaned

#' 
## ------------------------------------------------------------------------
names(FeaturesTrainData) <- NamesCleaned

#' 
## ------------------------------------------------------------------------
FeaturesTestData = FeaturesTestData[,SubsetNames]

#' 
## ------------------------------------------------------------------------
FeaturesTrainData = FeaturesTrainData[,SubsetNames]

#' 
#' 
#' cbind all training data
## ------------------------------------------------------------------------
TrainData <- cbind(as.data.table(SubjectTrainData),ActivityTrainData,FeaturesTrainData)
TrainData

#' 
#' cbind all test data
## ------------------------------------------------------------------------
TestData <- cbind(as.data.table(SubjectTestData),ActivityTestData,FeaturesTestData)

#' 
#' 
#' ### Merge test and train data to create one data set (point 1)
## ------------------------------------------------------------------------
Data <- rbind(TrainData,TestData)

#' 
## ------------------------------------------------------------------------
str(Data)

#' ### Uses descriptive activity names to name the activities in the data set (point 3)
#' 
#' This can be seen under "activity label" column in the MasterData dataset
#' 
#' Checking:
## ------------------------------------------------------------------------
levels(Data$activity)

#' Since we have a descriptive label, we can eliminate the "activity" column
#' 
## ------------------------------------------------------------------------
MasterData <- Data %>% select(-activityID)

#' 
## ------------------------------------------------------------------------
str(MasterData)

#' 
#' 
#' ### Appropriately labels the data set with descriptive variable names. (point 4)
#' 
#' Headings starting in f are frequency
#' Headings starting in t are time
#' "Acc" is an abbreviation for Accelerometer
#' "BodyBody" is changed to "Body"
#' "Gryo" is an abbreviation for Gyroscope
#' "Mag" is an abbreviation for Magnitude
#' "-" were removed from the mean, std. dev. and frequency.
## ------------------------------------------------------------------------
names(MasterData)<-gsub("^f", "Frequency", names(MasterData))
names(MasterData)<-gsub("^t", "Time", names(MasterData))
names(MasterData)<-gsub("Acc", "Accelerometer", names(MasterData))
names(MasterData)<-gsub("BodyBody", "Body", names(MasterData))
names(MasterData)<-gsub("Gyro", "Gyroscope", names(MasterData))
names(MasterData)<-gsub("Mag", "Magnitude", names(MasterData))
names(MasterData)<-gsub("-mean", "Mean", names(MasterData), ignore.case = TRUE)
names(MasterData)<-gsub("-std", "STD", names(MasterData), ignore.case = TRUE)
names(MasterData)<-gsub("-freq", "Frequency", names(MasterData), ignore.case = TRUE)

#' 
## ------------------------------------------------------------------------
str(MasterData)

#' 
#' ### From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. (Point 5)
## ------------------------------------------------------------------------
IndependentTidyData <- MasterData %>%
    group_by(subject, activity) %>%
    summarise_all(mean)

#' 
#' This output table qualifies as tidy data is it meets the requirements for tidy data described in Hadley Wickham's Tidy Data paper:
#' 1. Each variable forms a column.
#' 2. Each observation forms a row.
#' 3. Each type of observational unit forms a table.
#' 
#' In support of principle 1, each column refers to a different variable. In this case, the variables are the individual or subject, the activity undertaken by the subject, and the mean and standard deviation of various experimental parameters.
#' 
#' In support of principle 2, the rows consist of the subject number, the activity being performed, and the mean results of each variable being recorded for one subject undergoing one particular activity.
#' 
#' In support of principle 3, this data set can be used independently from the parent data set as it contains the mean performance across all of the measurements listed by subject and activity. It also contains observations of only one type.
#' 
## ------------------------------------------------------------------------
str(IndependentTidyData)

#' 
## ------------------------------------------------------------------------
IndependentTidyData

#' 
#' Export the file
## ------------------------------------------------------------------------
write.table(IndependentTidyData, file = "./IndependentTidyData.txt", row.names = FALSE)

#' 
#' 
#' 
#' 
