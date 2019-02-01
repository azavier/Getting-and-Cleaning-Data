library(plyr)
library(dplyr)


# 0. Downloading and Unzipping Dataset 

# Creating the directory to store the Dataset
if(!file.exists("./data")){dir.create("./data")}

# Downloading the course project data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/Dataset.zip", method="curl")

# Unzipping the downloaded Dataset
unzip(zipfile="./data/Dataset.zip", exdir="./data")


# 1. Merges the training and the test sets to create one data set 

# 1.1 Reading "features.txt" list
features <- read.table("./data/UCI HAR Dataset/features.txt")
colnames(features) <- c("n", "function")
#colnames(features)


# 1.2 Reading "activity_labels.txt"
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("id", "type")
#colnames(activityLabels)


# 1.3 Reading test data & assigning column names
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
colnames(subject_test) <- "subjectId"
#colnames(subject_test)

X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
colnames(X_test) <- features[,2]
#colnames(X_test)

y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
colnames(y_test) <- "activityId"
#colnames(y_test)


# 1.4 Reading train data & assigning column names
subject_training <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
colnames(subject_training) <- "subjectId"
#colnames(subject_training)

X_training <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
colnames(X_training) <- features[,2]
#colnames(X_training)

y_training <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
colnames(y_training) <- "activityId"
#colnames(y_training)


# 1.5 Merges the training and the test data to create one data set
mergedTest <- cbind(subject_test, y_test, X_test)
mergedTraining <- cbind(subject_training, y_training, X_training)

mergedData <- rbind(mergedTest, mergedTraining)
#dim(mergedData)
# [1] 10299   563

# 2. Extracts only the measurements on the mean and standard deviation 
#library(tidyr)

# 2.1 Getting the vector of the columns where the mean() and std() is stored
#     Note: we are taking mean() and std(), but not meanFreq()
mean_std <- which(grepl(".*mean*.|.*std*.", features[,2]) & !grepl(".*meanFreq*.", features[,2]))
mean_std

# 2.2 Creating tidy data set, with the specified measurements
#     Note: we are using mean_std+2, because at the beginning of the existing data set we have
#           'subjectId' and 'activityId' columns that are coming from subject and y data sets,
#           while 'features' is only referring to the column names of X data sets
tidyData <- tbl_df(mergedData[,c(1,2,mean_std+2)])
head(tidyData)


# 3. Uses descriptive activity names to name the activities in the-

tidyData$subjectId <- as.factor(tidyData$subjectId)
tidyData$activityId <- factor(tidyData$activityId, levels=activityLabels[,1], labels=activityLabels[,2])
head(tidyData)


# 4. Appropriately labels the data set with descriptive variable names 

names(tidyData)
names(tidyData)[2] <- "activity"
names(tidyData) <- gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData) <- gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData) <- gsub("^t", "time", names(tidyData))
names(tidyData) <- gsub("^f", "frequency", names(tidyData))
names(tidyData) <- gsub("BodyBody", "Body", names(tidyData))
names(tidyData) <- gsub("Mag-", "Magnitude-", names(tidyData))
names(tidyData) <- gsub("-mean()", "Mean", names(tidyData))
names(tidyData) <- gsub("-std()", "STD", names(tidyData))
names(tidyData)
str(tidyData)
dim(tidyData)
# dim(tidyData)
# [1] 10299   68


# 5. Creates a 2nd data set

finalDataSet <- tidyData %>% group_by(subjectId, activity) %>% summarise_all(mean)
str(finalDataSet)
head(finalDataSet)
dim(finalDataSet)
# dim(finalDataSet)
# [1] 180   68

# 5.2 Save the finalDataSet as a txt file
write.table(finalDataSet, "./data/finalDataSet.txt", row.name = FALSE)
