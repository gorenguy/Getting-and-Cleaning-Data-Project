#Download & unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("./data")) {dir.create("./data")}
path <- paste("./data/", "data.zip", sep = "")
library(downloader)
download(fileURL, dest = path, mode = "wb")
unzip("./data/data.zip", exdir = "./data")

#Assign paths
trainS_path <- paste(getwd(), "/data/UCI HAR Dataset/train/X_train.txt", sep = "")
trainL_path <- paste(getwd(), "/data/UCI HAR Dataset/train/y_train.txt", sep = "")
testS_path <- paste(getwd(), "/data/UCI HAR Dataset/test/X_test.txt", sep = "")
testL_path <- paste(getwd(), "/data/UCI HAR Dataset/test/y_test.txt", sep = "")
activityL_path <- paste(getwd(), "/data/UCI HAR Dataset/activity_labels.txt", sep = "")
features_path <- paste(getwd(), "/data/UCI HAR Dataset/features.txt", sep = "")
train_subjects_path <- paste(getwd(), "/data/UCI HAR Dataset/train/subject_train.txt", sep = "")
test_subjects_path <- paste(getwd(), "/data/UCI HAR Dataset/test/subject_test.txt", sep = "")

#Read data
trainS <- read.table(trainS_path)
trainL <- read.table(trainL_path)
testS <- read.table(testS_path)
testL <- read.table(testL_path)
activityL <- read.table(activityL_path)
features <- read.table(features_path)
train_subjects <- read.table(train_subjects_path)
test_subjects <- read.table(test_subjects_path)

# Merges the training and the test sets to create one data set
train <- cbind(train_subjects, trainL, trainS)
test <- cbind(test_subjects, testL, testS)
all <- rbind(train, test)

# Name the activities in the data set in descriptively.
all[, 2] <- as.character(all[, 2])
activityL[, 2] <- as.character(activityL[, 2])
for (i in 1:6) {
        all[, 2] <- replace(all[, 2], all[, 2] == i, activityL[i, 2])
}
# Appropriately labels the data set with descriptive variable names
vars <- as.character(features[, 2])
names(all) <- c("subject", "activity", vars)

# Extracts only the measurements on the mean and standard deviation for each measurement.
good <- grepl("mean|std|activity|subject", names(all))
tidy <- all[, good]

# Creates a second, independent tidy data set with the average of each variable for each activity and 
# each subject.
res <- ddply(tidy, .(subject, activity), function(x) colMeans(x[, 3:81]))

# Write table to a .txt file
write.table(res, "Results.txt", row.name=FALSE)
