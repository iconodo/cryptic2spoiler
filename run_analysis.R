setwd("D:/___CourSR/GandClData/wk4/UCI HAR Dataset")

library(dplyr)


#### trainers data ** pendiente quitar nrows = 50

trainersData <- read.table(
    "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/subject_train.txt", 
          header = FALSE)

names(trainersData) <- c("subject")

trainersData <- mutate(trainersData, 
                       subject = paste("person", subject, sep = ""))

#### testers data

testersData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/subject_test.txt", 
          header = FALSE)

names(testersData) <- c("subject")

testersData <- mutate(testersData, 
                       subject = paste("person", subject, sep = ""))



#### trainers activities

trainersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/y_train.txt",
          header = FALSE)

names(trainersActivitiesData) <- c("activity")

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


### testers Activities Data

testersActivitiesData <- read.table(
     "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/y_test.txt",
          header = FALSE)

names(testersActivitiesData) <- c("activity")

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


#### TRAINER accelerator XYZ data


trainData <- read.table(
          "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/train/X_train.txt",
               header = FALSE)

trainData <- select(trainData, 1:6)

names(trainData) <- c("meanx", "meany", "meanz", "stdevx", 
               "stdevy", "stdevz")

#### TESTER accelerator XYZ data
     
     
testData <- read.table(
             "D:/___CourSR/GandClData/wk4/UCI HAR Dataset/test/X_test.txt",
                 header = FALSE)

testData <- select(testData, 1:6)

names(testData) <- c("meanx", "meany", "meanz", "stdevx", 
                      "stdevy", "stdevz")


alltrainData <- cbind(trainersActivitiesData, trainersData, trainData)

alltestData <- cbind(testersActivitiesData, testersData, testData)

allAccData <- rbind(alltrainData, alltestData)

library(reshape2)

allAccMelt <- melt(allAccData, id = c("activity", "subject"), 
                   measure.vars = c("meanx", "meany", "meanz", "stdevx", 
                                    "stdevy", "stdevz"))

allAccMeltActAndSubjectInOne <- mutate(allAccMelt, actionwho = 
                           paste(activity, subject, sep = "-"))
                     
allAccMeltRidOfUnfusedVars <- 
     select(allAccMeltActAndSubjectInOne, actionwho, variable, value)                        

allAccMeltAndCast <- 
     dcast(allAccMeltRidOfUnfusedVars, actionwho ~ variable, mean)