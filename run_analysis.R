## selects working directory where data registers were unzipped

setwd("D:/___CourSR/GandClData/wk4/UCI HAR Dataset")

## loads 'dplyr' library for select(), mutate() functions

library(dplyr)

#### 1. MERGING THE TRAINING AND TEST DATA SETS

#### 1.1 Reads the trainers data set into R's objecet 'trainersData'

trainersData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/subject_train.txt", 
     header = FALSE)

#### 1.1.1  Labels the column as "subject",
#              adds prefix "trainer" to var's value

names(trainersData) <- c("subject")

trainersData <- mutate(trainersData, 
                       subject = paste("trainer", subject, sep = ""))

#### 1.1.2 Reads the testers data set into R's objet 'testersDtata' 

testersData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/subject_test.txt", 
          header = FALSE)

#### 1.1.2.1  Labels the var column as "subject",
#              adds prefix "tester" to var's value

names(testersData) <- c("subject")


testersData <- mutate(testersData, subject = 
                           paste("tester", subject, sep = "")) 

#### 1.2 Reads the trainers activities data into R's object
#                    'trainersActivitiesData'

trainersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/y_train.txt",
          header = FALSE)

###  labels var column as "activity"

names(trainersActivitiesData) <- c("activity")

###  1.2.1  substitutes var with values "walking", "walkingup", 
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


#### 1.3 Reads the testers activities data into R's object
#                    'testersActivitiesData'

testersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/y_test.txt",
          header = FALSE)

###  labels var column as "activity"

names(testersActivitiesData) <- c("activity")

###  1.3.1  substitutes var with values "walking", "walkingup", 
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


#### 1.4 Reads TRAINER's accelerometer XYZ data into R's object 'trainData'

trainData <- read.table(
          "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/X_train.txt",
               header = FALSE)

## selects/extracts columns for accelerometer's mean/standard deviation
##  x, y, z vectors

trainData <- select(trainData, 1:6)

## labels var columns 'meanx', 'meany', etc.

names(trainData) <- c("meanx", "meany", "meanz", "stdevx", 
               "stdevy", "stdevz")


#### 1.4 Reads TESTER's accelerometer XYZ data into R's object 'testData'

testData <- read.table(
             "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/X_test.txt",
                 header = FALSE)

## selects/extracts columns for accelerometer's mean/standard deviation
##  x, y, z vectors

testData <- select(testData, 1:6)

## labels var columns 'meanx', 'meany', etc.

names(testData) <- c("meanx", "meany", "meanz", "stdevx", 
                      "stdevy", "stdevz")

#### 1.5 Binds activities data, persons data, and 
##        registered vectors datasets
##        for both training and testing 
##        into R's objects 'alltrainData' and 'alltestData'

alltrainData <- cbind(trainersActivitiesData, trainersData, trainData)

alltestData <- cbind(testersActivitiesData, testersData, testData)

#### 1.6 Merges both datasets into R's object 'allAccData'

allAccData <- rbind(alltrainData, alltestData)

####  loads 'reshape2' library for melt(), dcast() functions

library(reshape2)

#### 1.7  melts data set by 'activity' + 'subject' vars/columns,
##        with the 6 variables mean/stdev paired

allAccMelt <- melt(allAccData, id = c("activity", "subject"), 
                   measure.vars = c("meanx", "stdevx", 
                                    "meany", "stdevy",
                                    "meanz", "stdevz"))

### 1.8   "Fuses" columns 'activity' and 'subject' into var/column
##                  named "whoseact" into R's object
##                  'allAccMeltActAndSubjectInOne'

allAccMeltActAndSubjectInOne <- mutate(allAccMelt, whoseact = 
                           paste(activity, subject, sep = "-"))
                     
### 1.9   Deletes vars/columns 'activity' and 'subject' and
##             reads the resuting dataset into R's object
##                  'allAccMeltRidOfUnfusedVars'

allAccMeltRidOfUnfusedVars <- 
     select(allAccMeltActAndSubjectInOne, whoseact, variable, value)                        

### 1.10 Casts the data set by mean/sdeviation values of var 'whoseact' 

accActsMeanSdTable <- 
     dcast(allAccMeltRidOfUnfusedVars, whoseact ~ variable, mean)


