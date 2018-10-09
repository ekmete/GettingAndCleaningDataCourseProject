#clear environment for the sake of performance improvement.
rm(list = ls())

#attach relevant package(s)
library(dplyr)


###############################################################
#Download and Unzip Dataset
###############################################################

if(!file.exists("./ProjectData")){
  dir.create("./ProjectData")
  }

zipUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile<-"./ProjectData/UCI HAR Dataset.zip"

if(!file.exists(zipFile)){
  download.file(zipUrl, zipFile, mode = "wb")
  }

unzip(zipFile, exdir = "./ProjectData")


###############################################################
#Read Relevant Files
###############################################################

features<-read.table("./ProjectData/UCI HAR Dataset/features.txt", as.is=TRUE, col.names=c("featureid", "feature"))

trainingSubject<-read.table("./ProjectData/UCI HAR Dataset/train/subject_train.txt", col.names="subject")
trainingValue<-read.table("./ProjectData/UCI HAR Dataset/train/X_train.txt", col.names=features$feature)
trainingLabel<-read.table("./ProjectData/UCI HAR Dataset/train/y_train.txt", col.names="activity")

testSubject<-read.table("./ProjectData/UCI HAR Dataset/test/subject_test.txt", col.names="subject")
testValue<-read.table("./ProjectData/UCI HAR Dataset/test/X_test.txt", col.names=features$feature)
testLabel<-read.table("./ProjectData/UCI HAR Dataset/test/y_test.txt",  col.names="activity")

activityLookup<-read.table("./ProjectData/UCI HAR Dataset/activity_labels.txt", 
                           col.names=c("activityid", "activity"))



###############################################################
# 1. Merges the training and the test sets to create one data set
###############################################################

activityData<-rbind(cbind(trainingSubject, trainingValue, trainingLabel), 
      cbind(testSubject,testValue,testLabel))


#previous files are not necessary. They are removed.

rm(trainingSubject, trainingValue, trainingLabel, 
   testSubject, testValue, testLabel)



###############################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
###############################################################

columns<-grep("subject|mean|std|activity", colnames(activityData))

activityData<-activityData[,columns]


###############################################################
# 3. Uses descriptive activity names to name the activities in the data set
###############################################################


activityData$activity <- factor(activityData$activity, 
                                 levels = activityLookup$activityid, labels = activityLookup$activity)


###############################################################
# 4. Appropriately labels the data set with descriptive variable names. 
###############################################################


activityDataCols<-colnames(activityData)

activityDataCols<-gsub("^t", "timeDomain", activityDataCols)

activityDataCols<-gsub("^f", "frequencyDomain", activityDataCols)

activityDataCols<-gsub("Acc", "Accelerometer", activityDataCols)

activityDataCols<-gsub("Gyro", "Gyroscope", activityDataCols)

activityDataCols<-gsub("Mag", "Magnitude", activityDataCols)

activityDataCols<-gsub("Freq", "Frequency", activityDataCols)

activityDataCols<-gsub("mean", "Mean", activityDataCols)

activityDataCols<-gsub("std", "StandardDeviation", activityDataCols)



activityDataCols<-gsub("BodyBody", "Body", activityDataCols)

activityDataCols<-gsub("[.]", "", activityDataCols)


colnames(activityData)<-activityDataCols



###############################################################
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
###############################################################


activityDataMeans<-activityData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(activityDataMeans, "./ProjectData/tidydata.txt", quote = FALSE, row.names = FALSE)