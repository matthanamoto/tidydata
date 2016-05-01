install.packages("data.table")
install.packages("dplyr")
install.packages("plyr")
install.packages("qdap")
install.packages("rJava")
library(data.table)
library(dplyr)
library(plyr)
library(rJava)
library(qdap)

# Set working directory to location of Samsung Data
setwd("C:\\Users\\Matt\\Downloads\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")

# Read in the feature names and activity labels (located in main folder).
# Create variable titles for activity labels for join function later in code.
activity.labels = read.table("activity_labels.txt")
colnames(activity.labels) = c("Activity", "ActivityName")

feature.names = read.table("features.txt")

# Reading in the subject, activity, and features training data (located in "training" folder)
subject.train  = read.table("train/subject_train.txt")
activity.train = read.table("train/y_train.txt")
features.train = read.table("train/x_train.txt")

# Reading in the subject, activity, and features test data (located in the "test" folder)
subject.test  = read.table("test/subject_test.txt")
activity.test = read.table("test/y_test.txt")
features.test = read.table("test/x_test.txt")

# 1: Merge the training and test sets to create a consolidated data set
subject.data  = rbind(subject.train, subject.test)
activity.data = rbind(activity.train, activity.test)
features.data = rbind(features.train, features.test)
# Add names from main folder to the feature data
colnames(features.data) = feature.names[,2]

# Add general column names to subject and activity sets
# Applied general names to make matching/joining easier in future.
colnames(subject.data) = "Subject"
colnames(activity.data) = "Activity"

# Create a consolidated data set of subject, activity, and features data
# Edited data property of activity and subject to factors instead of integers
All.Data = cbind(subject.data, activity.data, features.data)
All.Data$Activity = as.factor(All.Data$Activity)
All.Data$Subject = as.factor(All.Data$Subject)

# 2: Extract only the measurements on the mean and standard deviation for each measurement
Extracted.data = All.Data[,grep("Subject|Activity|mean|std", colnames(All.Data), ignore.case=TRUE)]
# Note: subject and activity columns were titled "Subject" and "Activity". I could have used
# the grep function to only extract mean and std, but including the subject and activity columns
# were relevant to the objective of the data extraction. Therefore, I included the subject
# and activity names to the grep function.

# 3: Use descriptive activity names to name the activities in the data set

ExtractAndActivity = join(activity.labels, Extracted.data, by="Activity")
# Join function did an excellent job of combining the two dataframes by the factor pattern
# in the activity.labels dataframe.

# 4: Appropriately label the data set with descriptive variable names.
# Per inspection of features_info.txt, noted that
# "Acc" could be relabeled as "Accelerometer",
# "Gyro" could be relabled as "Gyroscrope",
# Variables that began with "t" could be relabeled as "Time",
# Variables that began with "f" could be relabeled as "Frequency" (Freq was already used in variable names),
# "BodyBody"could be relabeled as simply "Body",
# Lastly, "Mag" could be relabeled as Magnitude.

# "Mag" and "Gyro" are fairly clear, but having them spelled out does aid in clarifying the meaning of the variable.

# Create two vectors for clean var names.
# Vec1 = pattern that needs to be relabeled
# Vec2 = what pattern needs to be changed to
var.rep.pattern = c("Acc", "Gyro", "BodyBody", "Mag")
var.rep.replacement = c("Accelerometer", "Gyroscope","Body", "Magnitude")
names(ExtractAndActivity) = mgsub(var.rep.pattern, var.rep.replacement,names(ExtractAndActivity))
# Used qdap package to access mgsub function. Allows you to use vector of patterns/replacements.

names(ExtractAndActivity) = gsub("^t", "Time", names(ExtractAndActivity))
names(ExtractAndActivity) = gsub("^f", "Frequency", names(ExtractAndActivity))
# Needed to do the Time and Frequency sub after batch replacement because they required "^" indicators

# 5: From the data set in step 4, create a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
Tidy.Data = aggregate(.~Subject+Activity, ExtractAndActivity, mean)
Tidy.Data$ActivityName=NULL
# aggregate used the functionality of the mean function across the Subjects and Activities.
# Unfortunately, it also removed activity names. I needed to rename the activities in this layer the same way I did earlier.
# I made sure to remove the old Activity Name variable before joining the datasets again.
Tidy.Data = join(activity.labels, Tidy.Data, by="Activity")
Tidy.Data = Tidy.Data[order(Tidy.Data$Subject, Tidy.Data$Activity),]
# Used order function to make the output cleaner and easier to comprehend.

write.table(Tidy.Data, file = "TidyData.txt", row.name=FALSE)
# Writes "Tidy data" to text file in working directory