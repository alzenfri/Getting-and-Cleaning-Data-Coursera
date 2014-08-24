##############################################################################################
## Coursera - Getting and Cleaning Data Course Project  - getdata-006

# This script will perform the following steps on the information downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##############################################################################################

# 0. Clean workspace and set working directory
wrkSpace<-ls();
rm(list=wrkSpace);
setwd("~/R-Coursera/Getting And Cleaning Data/CourseProject/UCI HAR Dataset");

# 1. Merge the training and the test sets to create one data set.
fts      <- read.table('./features.txt',header=FALSE);            
actType  <- read.table('./activity_labels.txt',header=FALSE);     
sbtTrain <- read.table('./train/subject_train.txt',header=FALSE); 
x_Train  <- read.table('./train/x_train.txt',header=FALSE);       
y_Train  <- read.table('./train/y_train.txt',header=FALSE);       

colnames(actType)       <- c('activityId','activityType');
colnames(sbtTrain)      <- "subjectId";
colnames(x_Train)       <- fts[,2]; 
colnames(y_Train)       <- "activityId";

trainData <- cbind(y_Train,sbtTrain,x_Train);

sbtTest     <- read.table('./test/subject_test.txt',header=FALSE); 
x_Test      <- read.table('./test/x_test.txt',header=FALSE);
y_Test      <- read.table('./test/y_test.txt',header=FALSE);

colnames(sbtTest)     <- "subjectId";
colnames(x_Test)      <- fts[,2]; 
colnames(y_Test)      <- "activityId";
tstData <- cbind(y_Test,sbtTest,x_Test);

finalData <- rbind(trainData,tstData);
clNames   <- colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
validCols <- grepl("mean\\(\\)|std\\(\\)|subjectId|activityId", clNames)
cleanData <- finalData[validCols==TRUE];

# 3. Use descriptive activity names to name the activities in the data set
cleanData <- merge(cleanData,actType,by='activityId',all.x=TRUE);
clNames  <- colnames(cleanData); 

# 4. Appropriately label the data set with descriptive activity names. 
for (i in 1:length(clNames)) 
{
  clNames[i] <- gsub("\\()","",clNames[i])
  clNames[i] <- gsub("-std$","StdDev",clNames[i])
  clNames[i] <- gsub("-mean","Mean",clNames[i])
  clNames[i] <- gsub("^(t)","Time",clNames[i])
  clNames[i] <- gsub("^(f)","Frequency",clNames[i])
  clNames[i] <- gsub("([Gg]ravity)","Gravity",clNames[i])
  clNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",clNames[i])
  clNames[i] <- gsub("[Gg]yro","Gyro",clNames[i])
  clNames[i] <- gsub("AccMag","AccMagnitude",clNames[i])
  clNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",clNames[i])
  clNames[i] <- gsub("JerkMag","JerkMagnitude",clNames[i])
  clNames[i] <- gsub("GyroMag","GyroMagnitude",clNames[i])
};
colnames(cleanData) <- clNames;

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
cleanDataNoAct  <- cleanData[,names(cleanData) != 'activityType'];
tidyData    <- aggregate(cleanDataNoAct[,names(cleanDataNoAct) != c('activityId','subjectId')],by=list(activityId=cleanDataNoAct$activityId,subjectId = cleanDataNoAct$subjectId),mean);
tidyData    <- merge(tidyData,actType,by='activityId',all.x=TRUE);
write.table(tidyData, './tidy_Data.txt',row.names=TRUE,sep='\t');