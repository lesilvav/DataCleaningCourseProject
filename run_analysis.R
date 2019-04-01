library(data.table)

pathToFolder <- file.path(getwd(),"UCI HAR Dataset")

# reading features and activity labels data
features <- fread(file.path(pathToFolder,"features.txt"), header = FALSE, col.names = c("featureId","featureName"))
labels <- fread(file.path(pathToFolder,"activity_labels.txt"), header = FALSE, col.names = c("activityId","activityName"))

####
# Goal 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
####

# getting the index of mean and std columns. 
meanStdColumns <- (grep("std|mean",as.vector(features$featureName)))

# reading training data but only mean and standard deviation columns
xTrain <- fread(file.path(pathToFolder,"train","X_train.txt"), header = FALSE, select = meanStdColumns)
yTrain <- fread(file.path(pathToFolder,"train","y_train.txt"), header = FALSE, col.names = c("activityId"))
subjectTrain <- fread(file.path(pathToFolder,"train","subject_train.txt"), header = FALSE, col.names = c("subjectId"))

# reading testing data but only mean and standard deviation columns
xTest <- fread(file.path(pathToFolder,"test","X_test.txt"), header = FALSE, select = meanStdColumns)
yTest <- fread(file.path(pathToFolder,"test","y_test.txt"), header = FALSE, col.names = c("activityId"))
subjectTest <- fread(file.path(pathToFolder,"test","subject_test.txt"), header = FALSE, col.names = c("subjectId"))


####
# Goal 1 - Merges the training and the test sets to create one data set.
####
trainMerged <- cbind(yTrain, subjectTrain, xTrain)
testMerged <- cbind(yTest, subjectTest, xTest)
singleData <- rbind(trainMerged, testMerged)


###
# Goal 3 - Uses descriptive activity names to name the activities in the data set
###
singleData <- merge(singleData, labels, by = c("activityId"))


###
# Goal 4 - Appropriately labels the data set with descriptive variable names.
###

#Selecting the mean and std features only 
featuresSelected <- filter(features, grepl("std|mean",featureName))
colnames(singleData)[3:81] <- featuresSelected$featureName
# moving activytName to convinient location
singleData <- select(singleData, activityId, activityName, subjectId, 3:81)

###
# Goal 5 - From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
###
newTidy <- aggregate(singleData, by=list(activity = singleData$activityName,subject = singleData$subjectId), FUN = mean)
newTidy <- select(newTidy, -(activityName:subjectId))
newTidy <- newTidy[order(newTidy$activityId, newTidy$subject),]

# saving the new tidy data
write.table(newTidy, "newTidy.txt", row.name = FALSE)
