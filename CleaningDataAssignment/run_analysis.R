## with assumption of copying the folders of "test" and "train" of the assignment into working directory

library(dplyr)
library(xlsx)
library(Hmisc)

######## Test data ############


## step1. Reading SubjectTest data and setting name of column 
SubjectTest <- read.table("test/subject_test.txt", sep = "")
SubjectTest <- setNames(SubjectTest, c("SubjectId")) 
head(SubjectTest)

## step2. Reading Activity type data and setting name of column 
ActivityVector <- read.table("test/y_test.txt", sep = "")
ActivityVector <- setNames(ActivityVector, c("ActivityId")) 
head(ActivityVector)

## step3. Reading test measurments from the file
Xtest <- read.table("test/X_test.txt", sep = "")

## step4. Now Building the Final Dataset for Test data According to instructions

## step4.1. Adding Subject Id data
FinalTest <- select(SubjectTest, SubjectId)

## step4.2. Adding ActivityId data
FinalTest <- cbind(FinalTest, ActivityVector)

## step4.3. Setting descriptive name for Activity type


FinalTest$ActivityName <- sapply(FinalTest$ActivityId, function (x){  if (x == 1) "WALKING" 
                                                                      else if (x == 2) "WALKING_UPSTAIRS"
                                                                      else if (x == 3) "WALKING_DOWNSTAIRS"
                                                                      else if (x == 4) "SITTING"
                                                                      else if (x == 5) "STANDING"
                                                                      else if (x == 6) "LAYING"
                                                                  })  
                                 
## step4.4. Extracting measurment of mean and standard deviation read from different sensors
##        and adding to FinalTest dataset

FinalTest <- cbind (FinalTest, "tBodyAcc-meanX" = Xtest[,1])
FinalTest <- cbind (FinalTest, "tBodyAcc-meanY" = Xtest[,2])
FinalTest <- cbind (FinalTest, "tBodyAcc-meanZ" = Xtest[,3])
FinalTest <- cbind (FinalTest, "tBodyAcc-stdX"  = Xtest[,4])
FinalTest <- cbind (FinalTest, "tBodyAcc-stdY"  = Xtest[,5])
FinalTest <- cbind (FinalTest, "tBodyAcc-stdZ"  = Xtest[,6])

FinalTest <- cbind (FinalTest, "tGravityAcc-meanX" = Xtest[,41])
FinalTest <- cbind (FinalTest, "tGravityAcc-meanY" = Xtest[,42])
FinalTest <- cbind (FinalTest, "tGravityAcc-meanZ" = Xtest[,43])
FinalTest <- cbind (FinalTest, "tGravityAcc-stdX" = Xtest[,44])
FinalTest <- cbind (FinalTest, "tGravityAcc-stdY" = Xtest[,45])
FinalTest <- cbind (FinalTest, "tGravityAcc-stdZ" = Xtest[,46])

FinalTest <- cbind (FinalTest, "tBodyAccJerk-meanX" = Xtest[,81])
FinalTest <- cbind (FinalTest, "tBodyAccJerk-meanY" = Xtest[,82])
FinalTest <- cbind (FinalTest, "tBodyAccJerk-meanZ" = Xtest[,83])
FinalTest <- cbind (FinalTest, "tBodyAccJerk-stdX" = Xtest[,84])
FinalTest <- cbind (FinalTest, "tBodyAccJerk-stdY" = Xtest[,85])
FinalTest <- cbind (FinalTest, "tBodyAccJerk-stdZ" = Xtest[,86])

FinalTest <- cbind (FinalTest, "tBodyGyro-meanX" = Xtest[,121])
FinalTest <- cbind (FinalTest, "tBodyGyro-meanY" = Xtest[,122])
FinalTest <- cbind (FinalTest, "tBodyGyro-meanZ" = Xtest[,123])
FinalTest <- cbind (FinalTest, "tBodyGyro-stdX" = Xtest[,124])
FinalTest <- cbind (FinalTest, "tBodyGyro-stdY" = Xtest[,125])
FinalTest <- cbind (FinalTest, "tBodyGyro-stdZ" = Xtest[,126])

FinalTest <- cbind (FinalTest, "tBodyGyroJerk-meanX" = Xtest[,161])
FinalTest <- cbind (FinalTest, "tBodyGyroJerk-meanY" = Xtest[,162])
FinalTest <- cbind (FinalTest, "tBodyGyroJerk-meanZ" = Xtest[,163])
FinalTest <- cbind (FinalTest, "tBodyGyroJerk-stdX" = Xtest[,164])
FinalTest <- cbind (FinalTest, "tBodyGyroJerk-stdY" = Xtest[,165])
FinalTest <- cbind (FinalTest, "tBodyGyroJerk-stdZ" = Xtest[,166])

FinalTest <- cbind (FinalTest, "tBodyAccMag-mean" = Xtest[,201])
FinalTest <- cbind (FinalTest, "tBodyAccMag-std" = Xtest[,202])

FinalTest <- cbind (FinalTest, "tGravityAccMag-mean" = Xtest[,214])
FinalTest <- cbind (FinalTest, "tGravityAccMag-std" = Xtest[,215])

FinalTest <- cbind (FinalTest, "tBodyAccJerkMag-mean" = Xtest[,227])
FinalTest <- cbind (FinalTest, "tBodyAccJerkMag-std" = Xtest[,228])

FinalTest <- cbind (FinalTest, "tBodyGyroMag-mean" = Xtest[,240])
FinalTest <- cbind (FinalTest, "tBodyGyroMag-std" = Xtest[,241])

FinalTest <- cbind (FinalTest, "tBodyGyroJerkMag-mean" = Xtest[,253])
FinalTest <- cbind (FinalTest, "tBodyGyroJerkMag-std" = Xtest[,254])

FinalTest <- cbind (FinalTest, "fBodyAcc-meanX" = Xtest[,266])
FinalTest <- cbind (FinalTest, "fBodyAcc-meanY" = Xtest[,267])
FinalTest <- cbind (FinalTest, "fBodyAcc-meanZ" = Xtest[,268])
FinalTest <- cbind (FinalTest, "fBodyAcc-stdX" = Xtest[,269])
FinalTest <- cbind (FinalTest, "fBodyAcc-stdY" = Xtest[,270])
FinalTest <- cbind (FinalTest, "fBodyAcc-stdZ" = Xtest[,271])


######## Training data ############

## step5. Reading SubjectTraining data and setting name of column 
SubjectTrain <- read.table("train/subject_train.txt", sep = " ")
SubjectTrain <- setNames(SubjectTrain, c("SubjectId")) 
head(SubjectTrain)

## step6. Reading Activity type data for traing dataset and setting name of column 
ActivityVectorTrain <- read.table("train/y_train.txt", sep = " ")
ActivityVectorTrain <- setNames(ActivityVectorTrain, c("ActivityId")) 
head(ActivityVectorTrain)

## step7. Reading test measurments from the file
Xtrain <- read.table("train/X_train.txt", sep = "")

## step8. Now Building the Final Dataset for Training data According to instructions

## step8.1. Adding Subject Id data
FinalTrain <- select(SubjectTrain, SubjectId)

## step8.2. Adding ActivityId data
FinalTrain <- cbind(FinalTrain, ActivityVectorTrain)

## step8.3. Setting descriptive name for Activity type


FinalTrain$ActivityName <- sapply(FinalTrain$ActivityId, function (x){  if (x == 1) "WALKING" 
                                                                      else if (x == 2) "WALKING_UPSTAIRS"
                                                                      else if (x == 3) "WALKING_DOWNSTAIRS"
                                                                      else if (x == 4) "SITTING"
                                                                      else if (x == 5) "STANDING"
                                                                      else if (x == 6) "LAYING"
})  


## step8.4. Extracting measurment of mean and standard deviation read from different sensors
##        and adding to FinalTraining dataset

FinalTrain <- cbind (FinalTrain, "tBodyAcc-meanX" = Xtrain[,1])
FinalTrain <- cbind (FinalTrain, "tBodyAcc-meanY" = Xtrain[,2])
FinalTrain <- cbind (FinalTrain, "tBodyAcc-meanZ" = Xtrain[,3])
FinalTrain <- cbind (FinalTrain, "tBodyAcc-stdX"  = Xtrain[,4])
FinalTrain <- cbind (FinalTrain, "tBodyAcc-stdY"  = Xtrain[,5])
FinalTrain <- cbind (FinalTrain, "tBodyAcc-stdZ"  = Xtrain[,6])

FinalTrain <- cbind (FinalTrain, "tGravityAcc-meanX" = Xtrain[,41])
FinalTrain <- cbind (FinalTrain, "tGravityAcc-meanY" = Xtrain[,42])
FinalTrain <- cbind (FinalTrain, "tGravityAcc-meanZ" = Xtrain[,43])
FinalTrain <- cbind (FinalTrain, "tGravityAcc-stdX" = Xtrain[,44])
FinalTrain <- cbind (FinalTrain, "tGravityAcc-stdY" = Xtrain[,45])
FinalTrain <- cbind (FinalTrain, "tGravityAcc-stdZ" = Xtrain[,46])

FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-meanX" = Xtrain[,81])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-meanY" = Xtrain[,82])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-meanZ" = Xtrain[,83])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-stdX" = Xtrain[,84])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-stdY" = Xtrain[,85])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerk-stdZ" = Xtrain[,86])

FinalTrain <- cbind (FinalTrain, "tBodyGyro-meanX" = Xtrain[,121])
FinalTrain <- cbind (FinalTrain, "tBodyGyro-meanY" = Xtrain[,122])
FinalTrain <- cbind (FinalTrain, "tBodyGyro-meanZ" = Xtrain[,123])
FinalTrain <- cbind (FinalTrain, "tBodyGyro-stdX" = Xtrain[,124])
FinalTrain <- cbind (FinalTrain, "tBodyGyro-stdY" = Xtrain[,125])
FinalTrain <- cbind (FinalTrain, "tBodyGyro-stdZ" = Xtrain[,126])

FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-meanX" = Xtrain[,161])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-meanY" = Xtrain[,162])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-meanZ" = Xtrain[,163])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-stdX" = Xtrain[,164])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-stdY" = Xtrain[,165])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerk-stdZ" = Xtrain[,166])

FinalTrain <- cbind (FinalTrain, "tBodyAccMag-mean" = Xtrain[,201])
FinalTrain <- cbind (FinalTrain, "tBodyAccMag-std" = Xtrain[,202])

FinalTrain <- cbind (FinalTrain, "tGravityAccMag-mean" = Xtrain[,214])
FinalTrain <- cbind (FinalTrain, "tGravityAccMag-std" = Xtrain[,215])

FinalTrain <- cbind (FinalTrain, "tBodyAccJerkMag-mean" = Xtrain[,227])
FinalTrain <- cbind (FinalTrain, "tBodyAccJerkMag-std" = Xtrain[,228])FinalTrain <- cbind (FinalTrain, "tBodyGyroMag-mean" = Xtrain[,240])
FinalTrain <- cbind (FinalTrain, "tBodyGyroMag-std" = Xtrain[,241])

FinalTrain <- cbind (FinalTrain, "tBodyGyroJerkMag-mean" = Xtrain[,253])
FinalTrain <- cbind (FinalTrain, "tBodyGyroJerkMag-std" = Xtrain[,254])

FinalTrain <- cbind (FinalTrain, "fBodyAcc-meanX" = Xtrain[,266])
FinalTrain <- cbind (FinalTrain, "fBodyAcc-meanY" = Xtrain[,267])
FinalTrain <- cbind (FinalTrain, "fBodyAcc-meanZ" = Xtrain[,268])
FinalTrain <- cbind (FinalTrain, "fBodyAcc-stdX" = Xtrain[,269])
FinalTrain <- cbind (FinalTrain, "fBodyAcc-stdY" = Xtrain[,270])
FinalTrain <- cbind (FinalTrain, "fBodyAcc-stdZ" = Xtrain[,271])


## step9. Building the Final Dataset by merging Test and Training data

FinalTest  <- cbind(FinalTest,"MeasureType" = "Test") 
FinalTrain <- cbind(FinalTrain,"MeasureType" = "Train")

FinalResult <- FinalTest
FinalResult <- rbind(FinalResult, FinalTrain)

## step10. Calculating average of each variable per subject per activity

FinalData <- aggregate(FinalResult[, 4:49], list(FinalResult$SubjectId, FinalResult$ActivityId, FinalResult$ActivityName), mean)

## setting the name of columns after aggregation

FinalData <- rename (FinalData, SubjectId = Group.1, ActivityId = Group.2, ActivityName = Group.3)
head(FinalData)


## step11. Writing the dataset into a text file
write.table(FinalData, file = "FinalResults.txt")
write.xlsx2(FinalData, file = "FinalResults.xlsx")







