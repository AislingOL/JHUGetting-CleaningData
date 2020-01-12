---
title: "CodeBook"
author: "AO"
date: "12/01/2020"
output: md_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#R Course Project: Getting and Cleaning Data Course Project Codebook

This codebook contains the guide to the work done, as well as the code used (Part I) and a description of the output (part II).

Please see the README.md for a description of the study design.

##Task:

You should create one R script called run_analysis.R that does the following. 
1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive variable names. 
5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###Please Note: These actions have not been performed in numerical order.

The lines of code at which the actions can be seen are highlighted in bold and contain a bracketed task number as a suffix.

Loading the libraries needed for this code
```{r}
library(data.table)
library(tidyverse)
```

Checking if the folder exists
```{r}
# Checking if the directory exists, creating one if not.
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl") 

# Unzipping the UNI HAR dataset folder.
if (!file.exists("UCI HAR Dataset")) { 
  unzip(zipfile="./data/Dataset.zip",exdir="./data") 
}

```
List the files in the folder
```{r}
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files
```


Read in the test and train data files.
```{r}
#Train data
FeaturesTrainData <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ActivityTrainData <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
SubjectTrainData <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")


#Test data
FeaturesTestData <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ActivityTestData <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
SubjectTestData <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

```

Read activity labels table.
```{r}
ActivityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")[,2]
```

Set activity labels (point 3)
```{r}
ActivityTestData[,2] = ActivityLabels[ActivityTestData[,1]]
names(ActivityTestData) = c("activityID", "activity")

ActivityTrainData[,2] = ActivityLabels[ActivityTrainData[,1]]
names(ActivityTrainData) = c("activityID", "activity")
```

Read features table

```{r}
FeaturesTable <- read.table("./data/UCI HAR Dataset/features.txt")[,2]
```

Keep only means and standard deviations from the Features table (Point 2)

```{r}
SubsetNames <-grepl("mean|std", FeaturesTable)
```

Assign varible names

```{r}
names(SubjectTestData) <-c("subject")
names(SubjectTrainData) <-c("subject")
names(FeaturesTestData) = FeaturesTable
names(FeaturesTrainData) = FeaturesTable
```

```{r}
NamesCleaned <- sapply(FeaturesTable, function(x) {gsub("[()]", "",x)})
names(FeaturesTestData) <- NamesCleaned
```

```{r}
names(FeaturesTrainData) <- NamesCleaned
```

```{r}
FeaturesTestData = FeaturesTestData[,SubsetNames]
```

```{r}
FeaturesTrainData = FeaturesTrainData[,SubsetNames]
```


cbind all training data
```{r}
TrainData <- cbind(as.data.table(SubjectTrainData),ActivityTrainData,FeaturesTrainData)
TrainData
```

cbind all test data
```{r}
TestData <- cbind(as.data.table(SubjectTestData),ActivityTestData,FeaturesTestData)
```


###Merge test and train data to create one data set (point 1)
```{r}
Data <- rbind(TrainData,TestData)
```

```{r}
str(Data)
```
###Uses descriptive activity names to name the activities in the data set (point 3)

This can be seen under "activity label" column in the MasterData dataset

Checking:
```{r}
levels(Data$activity)
```
Since we have a descriptive label, we can eliminate the "activity" column

```{r}
MasterData <- Data %>% select(-activityID)
```

```{r}
str(MasterData)
```


###Appropriately labels the data set with descriptive variable names. (point 4)

Headings starting in f are frequency
Headings starting in t are time
"Acc" is an abbreviation for Accelerometer
"BodyBody" is changed to "Body"
"Gryo" is an abbreviation for Gyroscope
"Mag" is an abbreviation for Magnitude
"-" were removed from the mean, std. dev. and frequency.
```{r}
names(MasterData)<-gsub("^f", "Frequency", names(MasterData))
names(MasterData)<-gsub("^t", "Time", names(MasterData))
names(MasterData)<-gsub("Acc", "Accelerometer", names(MasterData))
names(MasterData)<-gsub("BodyBody", "Body", names(MasterData))
names(MasterData)<-gsub("Gyro", "Gyroscope", names(MasterData))
names(MasterData)<-gsub("Mag", "Magnitude", names(MasterData))
names(MasterData)<-gsub("-mean", "Mean", names(MasterData), ignore.case = TRUE)
names(MasterData)<-gsub("-std", "STD", names(MasterData), ignore.case = TRUE)
names(MasterData)<-gsub("-freq", "Frequency", names(MasterData), ignore.case = TRUE)
```

```{r}
str(MasterData)
```

###From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. (Point 5)
```{r}
IndependentTidyData <- MasterData %>%
    group_by(subject, activity) %>%
    summarise_all(mean)
```

This output table qualifies as tidy data is it meets the requirements for tidy data described in Hadley Wickham's Tidy Data paper:
1. Each variable forms a column.
2. Each observation forms a row.
3. Each type of observational unit forms a table.

In support of principle 1, each column refers to a different variable. In this case, the variables are the individual or subject, the activity undertaken by the subject, and the mean and standard deviation of various experimental parameters.

In support of principle 2, the rows consist of the subject number, the activity being performed, and the mean results of each variable being recorded for one subject undergoing one particular activity.

In support of principle 3, this data set can be used independently from the parent data set as it contains the mean performance across all of the measurements listed by subject and activity. It also contains observations of only one type.

```{r}
str(IndependentTidyData)
```

```{r}
IndependentTidyData
```


```{r}
write.table(IndependentTidyData, file = "./IndependentTidyData.txt")
```

Generate codebook:
```{r}
library(knitr)
rmarkdown::render("Codebook.Rmd")
```
##Output

The output file is written to the IndependentTidyData.txt file. This data has 180 observations of 81 variables.

subject is the unique identifier for the individual undergoing observation (numbered 1-30).

activity is the text describing the type of activity the individual performed. There are six possible activities: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, and LAYING.

All other variables are the mean per subject per activity of the experimental parameter under investigation. The title of the variable corresponds to this experimental parameter.

