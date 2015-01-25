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
