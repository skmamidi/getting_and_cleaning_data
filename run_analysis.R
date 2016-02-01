#Step 0: Preliminary checks and data download

path <- getwd()
setwd(path)
if (!file.exists("./srcData.zip")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL,destfile="./srcData.zip",method="curl")
        unzip(zipfile = "srcData.zip", exdir = "./")
} 
        

#Step 1: Merge the training and the test sets to create one data set
#Step 1a: load the necessary libraries
        library(tidyr)        
        library(dplyr)
        library(data.table)

#Step 1b: read the necessary data into data tables
        tmpPath <- file.path(path, "UCI HAR Dataset", "train", "subject_train.txt")
        train_subject <- tbl_df(read.table(tmpPath))
       
        tmpPath <- file.path(path, "UCI HAR Dataset", "train", "X_train.txt")
        train_X <- tbl_df(read.table(tmpPath))
        
        tmpPath <- file.path(path, "UCI HAR Dataset", "train", "Y_train.txt")
        train_Y <- tbl_df(read.table(tmpPath))
        
        
        tmpPath <- file.path(path, "UCI HAR Dataset", "test", "subject_test.txt")
        test_subject <- tbl_df(read.table(tmpPath))
        
        tmpPath <- file.path(path, "UCI HAR Dataset", "test", "X_test.txt")
        test_X <- tbl_df(read.table(tmpPath))
        
        tmpPath <- file.path(path, "UCI HAR Dataset", "test", "Y_test.txt")
        test_Y <- tbl_df(read.table(tmpPath))
        
#Step 1c: add column 'testType' to the subject data, to indicate wheter the row corresponds to test/training data
        train_subject <- mutate(train_subject, testType = "Training Data")
        test_subject <- mutate(test_subject, testType = "Test Data")
        
#Step 1d: read the necessary data into data tables
        merged_subject <- rbind(train_subject, test_subject)
        merged_X <- rbind(train_X, test_X)
        merged_Y <- rbind(train_Y, test_Y)
        
        
        
#Step 1e: give logical names to columns
        setnames(merged_subject, "V1", "subject")
        setnames(merged_Y, "V1", "activityNum")
        
        tmpPath <- file.path(path, "UCI HAR Dataset",  "features.txt")
        featureNames <- tbl_df(read.table(tmpPath))
        setnames(featureNames, names(featureNames), c("featureNum", "featureName"))
        colnames(merged_X)<- featureNames$featureName
        
        merged_Subject_Y<- cbind(merged_subject, merged_Y)
        merged_data <- cbind(merged_Subject_Y, merged_X)
        
        
#Step 2: Extract only the measurements on the mean and standard deviation for each measurement
        colNames_Mean_StdDev <- grep("mean\\(\\)|std\\(\\)",featureNames$featureName,value=TRUE)
        colNames_Mean_StdDev <- union(c("subject","activityNum"), colNames_Mean_StdDev)
        dataTable_mean_stdDev <- subset(merged_data, select = colNames_Mean_StdDev)
        
#Step 3: Use descriptive activity names to name the activities in the data set
        tmpPath <- file.path(path, "UCI HAR Dataset",  "activity_labels.txt")
        activityLabels<- tbl_df(read.table(tmpPath))
        setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))
        
        dataTable_mean_stdDev <- merge(activityLabels, dataTable_mean_stdDev , by="activityNum", all.x=TRUE)

#Step 4: Appropriately label the data set with descriptive variable names
        
        names(dataTable_mean_stdDev)<-gsub("^t", "time", names(dataTable_mean_stdDev))
        names(dataTable_mean_stdDev)<-gsub("^f", "frequency", names(dataTable_mean_stdDev))
        names(dataTable_mean_stdDev)<-gsub("Acc", "Accelerometer", names(dataTable_mean_stdDev))
        names(dataTable_mean_stdDev)<-gsub("Gyro", "Gyroscope", names(dataTable_mean_stdDev))
        names(dataTable_mean_stdDev)<-gsub("Mag", "Magnitude", names(dataTable_mean_stdDev))
        names(dataTable_mean_stdDev)<-gsub("BodyBody", "Body", names(dataTable_mean_stdDev))
        

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
        dataTable_mean_stdDev_Aggr<- aggregate(. ~ subject - activityName, data = dataTable_mean_stdDev, mean) 
        dataTable_mean_stdDev_Aggr<- tbl_df(arrange(dataTable_mean_stdDev_Aggr,subject,activityName))

        
        write.table(dataTable_mean_stdDev_Aggr, file = "tidydata.txt",row.name=FALSE)
        

        
        
                
