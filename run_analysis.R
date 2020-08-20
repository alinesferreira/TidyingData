## In this assignment, we need to tidy data so it can be analyze following
## universally concepts of data frame.
########################################################################
### Question 1: merges the training and the test sets to create one data set.

# The first step is to set the working directory
setwd ("C:/Users/Aline Ferreira/Documents/AprendendoR/Course3/W4")

# Then, it is necessary to read in label files
actLab <- read.table ("./UCI HAR Dataset/activity_labels.txt", 
                     col.names = c ("activityLabels", "activityName"), quote ="")

# and to link the class names to their activities names
Feature <- read.table("./UCI HAR Dataset/features.txt",
                      col.names = c("featureLabels", "featureName"), quote = "")
# It would list all features

# The next step is to read test data
subjTest <- read.table ("./UCI HAR Dataset/test/subject_test.txt", col.names = c("subjectId"))
  # Each row corresponds to the subject who performed the activity for each
  #window sample
XTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
  # Measurements for the test data (features)
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
  # It returns the results for the test data

# So, it is necessary to combine all test data and attribute column names
colnames(XTest) <- Feature$featureName
colnames(yTest) <- c("activityLabels")
testData <-cbind (subjTest, XTest, yTest)

# We need to repeat the same commands to training data

#To read training data
subjTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = c("subjectId"))
  # Each row corresponds to the subject who performed the activity for each window
  # sample
XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
  # Measurements for the test data (features)
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
  # It returns the results for the training data

# So, it is necessary to combine all training data and attribute column names
colnames(XTrain) <- Feature$featureName
colnames(yTrain) <- c("activityLabels")
trainData <-cbind(subjTrain, XTrain, yTrain)

# Finally, test and train data are combined
allData <- rbind(testData,trainData)
# Then, one dataset with test and train data will be created.

#######################################################################
### Question 2: extracts only the measurements on the mean and standard 
#deviation for each measurement.

# To select only variables with mean and standard deviation while include the
# subject ID (column 1) and activity code (column 563)
meanSDdata <- allData[, c(1,grep(pattern = "mean\\(\\)|std\\(\\)", x= names 
                                 (allData)), 563)]


#######################################################################
### Question 3: uses descriptive activity names to name the activities in the 
#data set.

# Making a new column that consider the activityLabels a factor of 6 levels, and
#the label will be the same as the activity name
meanSDdata$subjectId <- as.factor(meanSDdata$subjectId)
meanSDdata$activity <- factor(meanSDdata$activityLabels,
                              levels = actLab$activityLabels,
                              labels = actLab$activityName)

#Then, the activity label column will be removed the keep data tidy
meanSDdata <- meanSDdata [, -68]
# Just to confirm that the column activityLabels was gone
names (meanSDdata)

#######################################################################
### Question 4: appropriately labels the data set with descriptive variable
#names.

# To remove the () for the mean and standard deviation values
colnames (meanSDdata) <-gsub (pattern = "\\(\\)", replacement = "", 
                              x = names (meanSDdata))
# To move the activity column to the second column
write.table(meanSDdata, file = "tidyData.txt", row.names = FALSE, quote = FALSE,
            sep = "\t")

#######################################################################
### Question 5: from the data set in step 4, creates a second, independent
# tidy data set with the average of each variable for each activity and
# each subject.

library(dplyr)
meanSDdataByIdAct <- group_by(meanSDdata, subjectId,activity) %>% 
                     summarise_all(funs(mean))
write.table(meanSDdataByIdAct,file = "tidyDataMean.txt", row.names = FALSE, 
            quote = FALSE, sep = "\t")

