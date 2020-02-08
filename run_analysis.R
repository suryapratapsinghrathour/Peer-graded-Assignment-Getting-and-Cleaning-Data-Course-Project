## Loading helpful prackages
	
	library(dplyr)
	
## Step 0- Downloading and Unzipping File
	
	filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
	if(!file.exists(filename)){
		fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
		download.file(fileURL, filename, method="curl")
	}
	# check if Directory Exists of Not
	if (!file.exists("UCI HAR Dataset")) { 
		unzip(filename) 
	}
	
	
## Assigning all the data frames

	features<-read.table('.\\features.txt',col.names=c('numb','features'))

	activity<-read.table('.\\activity_labels.txt',col.names=c('num','activityid'))

	subject_test<-read.table('.\\test\\subject_test.txt',col.names=c('subject'))

	x_test<-read.table('.\\test\\X_test.txt',col.names=features[,2])

	y_test<-read.table('.\\test\\y_test.txt',col.names=c('activityid'))

	y_train<-read.table('.\\train\\y_train.txt',col.names=c('activityid'))

	x_train<-read.table('.\\train\\X_train.txt',col.names=features[,2])

	subject_train<-read.table('.\\train\\subject_train.txt',col.names=c('subject'))
	
	
##  Step 1- Merges the training and the test sets to create one data set.
	## merging test data
	mrg_train <- cbind(y_train, subject_train, x_train)
	## merging train data
	mrg_test <- cbind(y_test, subject_test, x_test)
	## total merge
	totmrg <- rbind(mrg_train, mrg_test)
	

##  Step 2- Extracts only the measurements on the mean and standard deviation for each measurement.

	tidydata<-select(totmrg,subject,activityid,contains('mean'),contains('std'))
	

##  Step 3- Uses descriptive activity names to name the activities in the data set.
	
	tidydata$activityid<-activity[tidydata$activityid,2]
	

##  Step 4- Appropriately labels the data set with descriptive variable names

	names(tidydata)[2] = "activity"
	names(tidydata)<-gsub("Acc", ".Accelerometer", names(tidydata))
	names(tidydata)<-gsub("Gyro", ".Gyroscope", names(tidydata))
	names(tidydata)<-gsub("Body", "Body",names(tidydata))
	names(tidydata)<-gsub("BodyBody", ".Body", names(tidydata))
	names(tidydata)<-gsub("Mag", ".Magnitude", names(tidydata))
	names(tidydata)<-gsub("^t", "Time", names(tidydata))
	names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
	names(tidydata)<-gsub("-Mean()", "Mean", names(tidydata), ignore.case = TRUE)
	names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
	names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
	names(tidydata)<-gsub("angle", "Angle", names(tidydata))
	names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))
	

##  Step 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	
	FinalData <- tidydata %>% group_by(subject, activity) %>%
		summarise_all(funs(mean))


##  Creating New File FinalData

	write.table(FinalData, "FinalData.txt", row.name=FALSE)