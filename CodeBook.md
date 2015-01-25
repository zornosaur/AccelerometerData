###Data structure of tidyDat.txt

Note: imformation on data collection is copied from the original data set.

The variables in the tidyData.txt data set are from the "Human Activity Recognition Using Smartphones Dataset, Version 1.0"[^1] data. The set of variables include:

* Subject: The ID of the subject who completed the study
* Activity: The type of activity the subject was participating in

As well as the mean and standard deviation for a sereies of measurements selected for this database which come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The average of both the means and standard deviations for multiple trials for each of these trials where reported in each row for all combinations of subject and activity type.

###Data construction procedure

Data was compiled from the "Human Activity Recognition Using Smartphones Dataset, Version 1.0." The data set was constructed by combining both the "train" (UCI HAR Dataset/test/X_train.txt) and "test" (UCI HAR Dataset/test/X_test.txt) data. Then only variables which reported the standard deviation or mean where extracted and conbined with the Subject (UCI HAR Dataset/test/subject_test.txt, UCI HAR Dataset/test/subject_train.txt) and Activity (UCI HAR Dataset/test/y_test.txt, UCI HAR Dataset/test/y_train.txt) data. Activity identifiers were then replaced with corresponding activity names from the file (UCI HAR Dataset/activity_labels.txt). A "tidy" data set was then created by taking every combination of subject and activity and reporting the mean for all the trials of each variable. This data set was exported as tidyData.txt. 

Code executed (assuming data set is in working directory):

```r
#This script imports data from Samsung Galaxy S acceleromters, 
#extracts the mean and standard deviation variables, labels 
#the variables, and outputs a tidy data set as a text file 
#which lists the mean for each valuable for every combination
#of subject and activity

#read in data to variables
features = read.table("UCI HAR Dataset/features.txt")
features = features$V2
labels = read.table("UCI HAR Dataset/activity_labels.txt")
labels = labels$V2

y_train = read.table("UCI HAR Dataset/train/y_train.txt")
x_train = read.table("UCI HAR Dataset/train/X_train.txt")
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt")

y_test = read.table("UCI HAR Dataset/test/y_test.txt")
x_test = read.table("UCI HAR Dataset/test/X_test.txt")
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt")

#conbine train and test data

x = rbind(x_train, x_test)
subject = rbind(subject_train, subject_test)
y = rbind(y_train, y_test)

#conbine this data with features and subject

conbinedTable = cbind(subject, y, x)

#Set variable names to something meaningful

colnames(conbinedTable)[1:2] = c("Subject","Activity")
colnames(conbinedTable)[3:563] <-as.character(features)

#Extract only mean and standard deviation values

temp = conbinedTable[grepl("mean()",colnames(conbinedTable))|grepl("std()",colnames(conbinedTable))|grepl("Subject",colnames(conbinedTable))|grepl("Activity",colnames(conbinedTable))]
meanSDOnly = temp[!grepl("meanFreq()",colnames(temp))] #We don't want the meanFreq values

#Rename Activities from numbers to activity type

for (i in 1:6){
  meanSDOnly$Activity[meanSDOnly$Activity == i] <- as.character(labels)[i]
}

#Create tidy data

tidyData = aggregate(. ~ Subject+Activity,data = meanSDOnly,FUN=function(meanSDOnly) mn =mean(meanSDOnly))

#Write tidy data set to file

write.table(tidyData, file = "tidyData.txt", row.names = F)

```

[^1]: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012. This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited. Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
