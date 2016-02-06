## selects working directory where data registers were unzipped

setwd("D:/___CourSR/GandClData/wk4/UCI HAR Dataset")

## loads 'dplyr' library for select(), mutate() functions

library(dplyr)

#### 1. MERGING THE TRAINING AND TEST DATA SETS

#### 1.1 Reads/process the trainers data set into R's objecet 'trainersData'

trainersData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/subject_train.txt", 
     header = FALSE)


#### 1.1.1  Creates empty column, labels the column as "subject",
#              deletes old colum, and
#              adds prefix "trainer" to var's value

trainersData <- mutate(trainersData, subject = "")


j <- nrow(trainersData)

for (i in 1:j){
     if (trainersData[i, 1] < 10) {
          trainersData[i, 2] <- paste("0", trainersData[i, 1], sep = "")
     } else {
          trainersData[i, 2] <- paste("", trainersData[i, 1], sep = "")
     }
}

trainersData <- select(trainersData, subject)

## names(trainersData) <- c("subject")

trainersData <- mutate(trainersData, 
                       subject = paste("trainer", subject, sep = ""))

#### 1.2 Reads/process the testers data set into R's objet 'testersDtata' 

testersData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/subject_test.txt", 
     header = FALSE)



#### 1.2.1  Creates empty column, labels the column as "subject",
#              deletes old colum, and
#              adds prefix "tester" to var's value

testersData <- mutate(testersData, subject = "")


k <- nrow(testersData)

for (i in 1:k){
     if (testersData[i, 1] < 10) {
          testersData[i, 2] <- paste("0", testersData[i, 1], sep = "")
     } else {
          testersData[i, 2] <- paste("", testersData[i, 1], sep = "")
     }
}

testersData <- select(testersData, subject)

## names(trainersData) <- c("subject")

testersData <- mutate(testersData, 
                      subject = paste("tester", subject, sep = ""))


#### 1.3 Reads/process the trainers activities data into R's object
#                    'trainersActivitiesData'

trainersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/y_train.txt",
          header = FALSE)

###  labels var column as "activity"

names(trainersActivitiesData) <- c("activity")

###  1.3.1  substitutes var with values "walking", "walkingup", 
#             "walkingdn", "sitting", "standing", "laying"

for(i in 1:nrow(trainersActivitiesData)) {
     if(trainersActivitiesData$activity[i] == 1) {
          trainersActivitiesData$activity[i] <- "walking" 
     } else if(trainersActivitiesData$activity[i] == 2) {
          trainersActivitiesData$activity[i] <- "walkingup"
     } else if(trainersActivitiesData$activity[i] == 3) {
          trainersActivitiesData$activity[i] <- "walkingdn"
     } else if(trainersActivitiesData$activity[i] == 4) {
          trainersActivitiesData$activity[i] <- "sitting"
     } else if(trainersActivitiesData$activity[i] == 5) {
          trainersActivitiesData$activity[i] <- "standing"          
     } else {
          trainersActivitiesData$activity[i] <- "laying"      
     }
}


#### 1.4 Reads/process the testers activities data into R's object
#                    'testersActivitiesData'

testersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/y_test.txt",
          header = FALSE)

###  labels var column as "activity"

names(testersActivitiesData) <- c("activity")

###  1.4.1  substitutes var with values "walking", "walkingup", 
#             "walkingdn", "sitting", "standing", "laying"

for(i in 1:nrow(testersActivitiesData)) {
     if(testersActivitiesData$activity[i] == 1) {
          testersActivitiesData$activity[i] <- "walking" 
     } else if(testersActivitiesData$activity[i] == 2) {
          testersActivitiesData$activity[i] <- "walkingup"
     } else if(testersActivitiesData$activity[i] == 3) {
          testersActivitiesData$activity[i] <- "walkingdn"
     } else if(testersActivitiesData$activity[i] == 4) {
          testersActivitiesData$activity[i] <- "sitting"
     } else if(testersActivitiesData$activity[i] == 5) {
          testersActivitiesData$activity[i] <- "standing"          
     } else {
          testersActivitiesData$activity[i] <- "laying"      
     }
}


#### 1.5 Reads/process TRAINER's accelerometer XYZ data 
##       into R's object 'trainData'

trainData <- read.table(
          "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/X_train.txt",
               header = FALSE)

## selects/extracts columns for accelerometer's mean/standard deviation
##  x, y, z vectors

selectVect <- c(1, 2, 3, 4, 5, 6, 41, 42, 43, 44, 45, 46, 81, 82, 83, 
                84, 85, 86, 121, 122, 123, 124, 125, 126, 161, 162, 163,
                164, 165, 166, 201, 202, 214, 215, 227, 228, 240, 241,
                253, 254, 266, 267, 268, 269, 270, 271, 345, 346, 347,
                348, 349, 350, 424, 425, 426, 427, 428, 429, 503, 504,
                516, 517, 529, 530, 542, 543)

trainData <- select(trainData, selectVect)

## labels var columns '[....]-mean', '[....]-std', etc.

names(trainData) <- 
     c('volunteer-accelerometer-x-velocitychange-mean',
       'volunteer-accelerometer-y-velocitychange-mean',
       'volunteer-accelerometer-z-velocitychange-mean',
       'volunteer-accelerometer-x-velocitychange-std',
       'volunteer-accelerometer-y-velocitychange-std',
       'volunteer-accelerometer-z-velocitychange-std',
       'gravity-accelerometer-x-velocitychange-mean',  
       'gravity-accelerometer-y-velocitychange-mean',
       'gravity-accelerometer-z-velocitychange-mean',
       'gravity-accelerometer-x-velocitychange-std',
       'gravity-accelerometer-y-velocitychange-std',
       'gravity-accelerometer-z-velocitychange-std',
       'volunteer-accelerometer-x-accelerationchange-mean',
       'volunteer-accelerometer-y-accelerationchange-mean',
       'volunteer-accelerometer-z-accelerationchange-mean',
       'volunteer-accelerometer-x-accelerationchange-std',
       'volunteer-accelerometer-y-accelerationchange-std',
       'volunteer-accelerometer-z-accelerationchange-std',
       'volunteer-gyroscope-x-angularvelocitychange-mean',
       'volunteer-gyroscope-y-angularvelocitychange-mean',
       'volunteer-gyroscope-z-angularvelocitychange-mean',
       'volunteer-gyroscope-x-angularvelocitychange-std',
       'volunteer-gyroscope-y-angularvelocitychange-std',
       'volunteer-gyroscope-z-angularvelocitychange-std',
       'volunteer-gyroscope-x-angularaccelerationchange-mean',
       'volunteer-gyroscope-y-angularaccelerationchange-mean',
       'volunteer-gyroscope-z-angularaccelerationchange-mean',
       'volunteer-gyroscope-x-angularaccelerationchange-std',
       'volunteer-gyroscope-y-angularaccelerationchange-std',
       'volunteer-gyroscope-z-angularaccelerationchange-std',
       'volunteer-accelerometer-velocity-magnitudechange-mean',
       'volunteer-accelerometer-velocity-magnitudechange-std',
       'gravity-accelerometer-velocity-magnitudechange-mean',
       'gravity-accelerometer-velocity-magnitudechange-std',
       'volunteer-accelerometer-acceleration-magnitudechange-mean',
       'volunteer-accelerometer-acceleration-magnitudechange-std',
       'volunteer-gyroscope-angularspeed-magnitudechange-mean',
       'volunteer-gyroscope-angularspeed-magnitudechange-std',
       'volunteer-gyroscope-angularacceleration-magnitudechange-mean',
       'volunteer-gyroscope-angularacceleration-magnitudechange-std',
       'volunteer-accelerometer-x-angularspeed-mean',
       'volunteer-accelerometer-y-angularspeed-mean',
       'volunteer-accelerometer-z-angularspeed-mean',
       'volunteer-accelerometer-x-angularspeed-std',
       'volunteer-accelerometer-y-angularspeed-std',
       'volunteer-accelerometer-z-angularspeed-std',
       'volunteer-accelerometer-x-angularspeedchange-mean',
       'volunteer-accelerometer-y-angularspeedchange-mean',
       'volunteer-accelerometer-z-angularspeedchange-mean',
       'volunteer-accelerometer-x-angularspeedchange-std',
       'volunteer-accelerometer-y-angularspeedchange-std',
       'volunteer-accelerometer-z-angularspeedchange-std',
       'volunteer-gyroscope-x-angularspeed-mean',
       'volunteer-gyroscope-y-angularspeed-mean',
       'volunteer-gyroscope-z-angularspeed-mean',
       'volunteer-gyroscope-x-angularspeed-std',
       'volunteer-gyroscope-y-angularspeed-std',
       'volunteer-gyroscope-z-angularspeed-std',
       'volunteer-accelerometer-angularspeed-magnitude-mean',
       'volunteer-accelerometer-angularspeed-magnitude-std',
       'volunteer-volunteer-accelerometer-angularspeedchange-mean',
       'volunteer-volunteer-accelerometer-angularspeedchange-std',
       'volunteer-gyroscope-angularspeed-magnitude-mean',
       'volunteer-gyroscope-angularspeed-magnitude-std',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-mean',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-std')     


#### 1.6 Reads/process TESTER's accelerometer XYZ data 
##       into R's object 'testData'

testData <- read.table(
             "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/X_test.txt",
                 header = FALSE)

## selects/extracts columns for accelerometer's mean/standard deviation
##  x, y, z vectors

testData <- select(testData, selectVect)

## labels var columns '[....]-mean', '[....]-mean', etc.

names(testData) <-
     c('volunteer-accelerometer-x-velocitychange-mean',
       'volunteer-accelerometer-y-velocitychange-mean',
       'volunteer-accelerometer-z-velocitychange-mean',
       'volunteer-accelerometer-x-velocitychange-std',
       'volunteer-accelerometer-y-velocitychange-std',
       'volunteer-accelerometer-z-velocitychange-std',
       'gravity-accelerometer-x-velocitychange-mean',  
       'gravity-accelerometer-y-velocitychange-mean',
       'gravity-accelerometer-z-velocitychange-mean',
       'gravity-accelerometer-x-velocitychange-std',
       'gravity-accelerometer-y-velocitychange-std',
       'gravity-accelerometer-z-velocitychange-std',
       'volunteer-accelerometer-x-accelerationchange-mean',
       'volunteer-accelerometer-y-accelerationchange-mean',
       'volunteer-accelerometer-z-accelerationchange-mean',
       'volunteer-accelerometer-x-accelerationchange-std',
       'volunteer-accelerometer-y-accelerationchange-std',
       'volunteer-accelerometer-z-accelerationchange-std',
       'volunteer-gyroscope-x-angularvelocitychange-mean',
       'volunteer-gyroscope-y-angularvelocitychange-mean',
       'volunteer-gyroscope-z-angularvelocitychange-mean',
       'volunteer-gyroscope-x-angularvelocitychange-std',
       'volunteer-gyroscope-y-angularvelocitychange-std',
       'volunteer-gyroscope-z-angularvelocitychange-std',
       'volunteer-gyroscope-x-angularaccelerationchange-mean',
       'volunteer-gyroscope-y-angularaccelerationchange-mean',
       'volunteer-gyroscope-z-angularaccelerationchange-mean',
       'volunteer-gyroscope-x-angularaccelerationchange-std',
       'volunteer-gyroscope-y-angularaccelerationchange-std',
       'volunteer-gyroscope-z-angularaccelerationchange-std',
       'volunteer-accelerometer-velocity-magnitudechange-mean',
       'volunteer-accelerometer-velocity-magnitudechange-std',
       'gravity-accelerometer-velocity-magnitudechange-mean',
       'gravity-accelerometer-velocity-magnitudechange-std',
       'volunteer-accelerometer-acceleration-magnitudechange-mean',
       'volunteer-accelerometer-acceleration-magnitudechange-std',
       'volunteer-gyroscope-angularspeed-magnitudechange-mean',
       'volunteer-gyroscope-angularspeed-magnitudechange-std',
       'volunteer-gyroscope-angularacceleration-magnitudechange-mean',
       'volunteer-gyroscope-angularacceleration-magnitudechange-std',
       'volunteer-accelerometer-x-angularspeed-mean',
       'volunteer-accelerometer-y-angularspeed-mean',
       'volunteer-accelerometer-z-angularspeed-mean',
       'volunteer-accelerometer-x-angularspeed-std',
       'volunteer-accelerometer-y-angularspeed-std',
       'volunteer-accelerometer-z-angularspeed-std',
       'volunteer-accelerometer-x-angularspeedchange-mean',
       'volunteer-accelerometer-y-angularspeedchange-mean',
       'volunteer-accelerometer-z-angularspeedchange-mean',
       'volunteer-accelerometer-x-angularspeedchange-std',
       'volunteer-accelerometer-y-angularspeedchange-std',
       'volunteer-accelerometer-z-angularspeedchange-std',
       'volunteer-gyroscope-x-angularspeed-mean',
       'volunteer-gyroscope-y-angularspeed-mean',
       'volunteer-gyroscope-z-angularspeed-mean',
       'volunteer-gyroscope-x-angularspeed-std',
       'volunteer-gyroscope-y-angularspeed-std',
       'volunteer-gyroscope-z-angularspeed-std',
       'volunteer-accelerometer-angularspeed-magnitude-mean',
       'volunteer-accelerometer-angularspeed-magnitude-std',
       'volunteer-volunteer-accelerometer-angularspeedchange-mean',
       'volunteer-volunteer-accelerometer-angularspeedchange-std',
       'volunteer-gyroscope-angularspeed-magnitude-mean',
       'volunteer-gyroscope-angularspeed-magnitude-std',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-mean',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-std')     


#### 1.7 Binds activities data, persons data, and 
##        registered vectors datasets
##        for both training and testing 
##        into R's objects 'alltrainData' and 'alltestData'

alltrainData <- cbind(trainersActivitiesData, trainersData, trainData)

alltestData <- cbind(testersActivitiesData, testersData, testData)

#### 1.8 Merges both datasets into R's object 'allAccData'

allAccData <- rbind(alltrainData, alltestData)

####  loads 'reshape2' library for melt(), dcast() functions

library(reshape2)

#### 1.9  melts data set by 'activity' + 'subject' vars/columns,
##        with the 33 variables mean/stdev paired

meltVect <- 
     c('volunteer-accelerometer-x-velocitychange-mean',
       'volunteer-accelerometer-x-velocitychange-std',
       'volunteer-accelerometer-y-velocitychange-mean',
       'volunteer-accelerometer-y-velocitychange-std',
       'volunteer-accelerometer-z-velocitychange-mean',
       'volunteer-accelerometer-z-velocitychange-std',
       'gravity-accelerometer-x-velocitychange-mean',  
       'gravity-accelerometer-x-velocitychange-std',
       'gravity-accelerometer-y-velocitychange-mean',
       'gravity-accelerometer-y-velocitychange-std',
       'gravity-accelerometer-z-velocitychange-mean',
       'gravity-accelerometer-z-velocitychange-std',
       'volunteer-accelerometer-x-accelerationchange-mean',
       'volunteer-accelerometer-x-accelerationchange-std',
       'volunteer-accelerometer-y-accelerationchange-mean',
       'volunteer-accelerometer-y-accelerationchange-std',
       'volunteer-accelerometer-z-accelerationchange-mean',
       'volunteer-accelerometer-z-accelerationchange-std',
       'volunteer-gyroscope-x-angularvelocitychange-mean',
       'volunteer-gyroscope-x-angularvelocitychange-std',
       'volunteer-gyroscope-y-angularvelocitychange-mean',
       'volunteer-gyroscope-y-angularvelocitychange-std',
       'volunteer-gyroscope-z-angularvelocitychange-mean',
       'volunteer-gyroscope-z-angularvelocitychange-std',
       'volunteer-gyroscope-x-angularaccelerationchange-mean',
       'volunteer-gyroscope-x-angularaccelerationchange-std',
       'volunteer-gyroscope-y-angularaccelerationchange-mean',
       'volunteer-gyroscope-y-angularaccelerationchange-std',
       'volunteer-gyroscope-z-angularaccelerationchange-mean',
       'volunteer-gyroscope-z-angularaccelerationchange-std',
       'volunteer-accelerometer-velocity-magnitudechange-mean',
       'volunteer-accelerometer-velocity-magnitudechange-std',
       'gravity-accelerometer-velocity-magnitudechange-mean',
       'gravity-accelerometer-velocity-magnitudechange-std',
       'volunteer-accelerometer-acceleration-magnitudechange-mean',
       'volunteer-accelerometer-acceleration-magnitudechange-std',
       'volunteer-gyroscope-angularspeed-magnitudechange-mean',
       'volunteer-gyroscope-angularspeed-magnitudechange-std',
       'volunteer-gyroscope-angularacceleration-magnitudechange-mean',
       'volunteer-gyroscope-angularacceleration-magnitudechange-std',
       'volunteer-accelerometer-x-angularspeed-mean',
       'volunteer-accelerometer-x-angularspeed-std',
       'volunteer-accelerometer-y-angularspeed-mean',
       'volunteer-accelerometer-y-angularspeed-std',
       'volunteer-accelerometer-z-angularspeed-mean',
       'volunteer-accelerometer-z-angularspeed-std',
       'volunteer-accelerometer-x-angularspeedchange-mean',
       'volunteer-accelerometer-x-angularspeedchange-std',
       'volunteer-accelerometer-y-angularspeedchange-mean',
       'volunteer-accelerometer-y-angularspeedchange-std',
       'volunteer-accelerometer-z-angularspeedchange-mean',
       'volunteer-accelerometer-z-angularspeedchange-std',
       'volunteer-gyroscope-x-angularspeed-mean',
       'volunteer-gyroscope-x-angularspeed-std',
       'volunteer-gyroscope-y-angularspeed-mean',
       'volunteer-gyroscope-y-angularspeed-std',
       'volunteer-gyroscope-z-angularspeed-mean',
       'volunteer-gyroscope-z-angularspeed-std',
       'volunteer-accelerometer-angularspeed-magnitude-mean',
       'volunteer-accelerometer-angularspeed-magnitude-std',
       'volunteer-volunteer-accelerometer-angularspeedchange-mean',
       'volunteer-volunteer-accelerometer-angularspeedchange-std',
       'volunteer-gyroscope-angularspeed-magnitude-mean',
       'volunteer-gyroscope-angularspeed-magnitude-std',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-mean',
       'volunteer-volunteer-gyroscope-angularspeed-magnituedchange-std')

allAccMelt <- melt(allAccData, id = c("activity", "subject"), 
  measure.vars = meltVect)

### 1.10   "Fuses" columns 'activity' and 'subject' into var/column
##                  named "whoseact" into R's object
##                  'allAccMeltActAndSubjectInOne'

allAccMeltActAndSubjectInOne <- mutate(allAccMelt, whoseact = 
                           paste(activity, subject, sep = "-"))
                     
### 1.11   Deletes vars/columns 'activity' and 'subject' and
##             reads the resuting dataset into R's object
##                  'allAccMeltRidOfUnfusedVars'

allAccMeltRidOfUnfusedVars <- 
     select(allAccMeltActAndSubjectInOne, whoseact, variable, value)                        

### 1.12 Casts the data set by mean/sdeviation values of var 'whoseact'
##       into R's object accActsMeanSdTable

accActsMeanSdTable <- 
     dcast(allAccMeltRidOfUnfusedVars, whoseact ~ variable, mean)


