## Loading the dplyr package
library("dplyr")

## STEP 1.
## I will read each dataset in turn. 
## I'll take the 'features.txt' data first because this file contains variable names.
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)

## The data needs some modification (I'll remove the brackets and present the data as factors). 
## I add another variable, so the original variable remains the original one.
features <- cbind(features, names=gsub("()","",features$V2, fixed=TRUE), stringsAsFactors=FALSE)

## Second, I'll upload the rest of the data.
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$names)
X_test <-  read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$names)
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt", col.names = "activity")
Y_test <-  read.table("UCI HAR Dataset/test/Y_test.txt", col.names = "activity")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("V1", "activity.name"))

## Let's create two datasets. The first set of data is training data. The second dataset is a test. 
## Where the activity-levels and subject are combined with the measurements.
testdata <- cbind(Y_test, subject_test, X_test)
traindata <- cbind(Y_train, subject_train, X_train)

## Now I will combine the two datasets into one.
full_data <- rbind(traindata, testdata)

## Now I check if the "full_data" dataset contains any missing values.
any(is.na(full_data))

## Since there are no missing values in this dataset, I move on to the second step.

## STEP 2.
## I will extract only the measurements about the mean 
## and standard deviation for each measurement. 
## I extract only the variables (columns) with the name "mean" an "std" in it.
mean_and_std <- select(full_data, activity, subject, contains("mean"), contains("std"))

## STEP 3: Uses descriptive activity names to name the activities in 
## the data set. 
## I merge the "mean_and_std" with the "activity_labels", where the 
## key in "mean_and_std"(by.x) is "activity" and the key in 
## "activity_labels" (by.y) is "V1".
merged <- merge(activity_labels, mean_and_std, by.y = "activity", by.x = "V1", sort = FALSE)

## Because this dataset contains two variables that holds the same kind of data,
## i.e. "V1" and "activity.name", it does not meet the rules of a tidy data set.
## So I remove the column "V1".
merged <- select(merged,-V1)

## STEP 4. For the colnames to be tidy, I replaced all '-' for "_". Also there
## were some names with 'Bodybody' in it, so I changed it to 'Body'.
colnames(merged) <- gsub("[-]", "_", colnames(merged))
colnames(merged) <- gsub("Bodybody", "Body", colnames(merged))


## STEP 5. From 'merged' I create 'group_data_mean'by grouping
## by activity.name and subject. Next I summerise all variables using the
## 'summirise_each' command. Then I write this to the file run_data_mean.txt.
group_data_mean <- merged %>% group_by(activity.name, subject) %>% summarise_each(funs(mean))

write.table(group_data_mean, file="run_data_mean.txt", row.name=FALSE)
