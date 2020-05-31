run_analysis <- function() {
    
    create_data_set <- function(type) {
        
        startFilePath <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
        fileExtension <- ".txt"
        
        ## read activity data set
        filePath <- paste0(startFilePath, type, "/y_", type, fileExtension)
        activityDataSet <- read.table(filePath, header=FALSE, col.names = "activityCode")
        
        ## left outer join activity labels with activityDataSet to match the activity labels
        library(plyr)
        activityDataSetWithLabel <- join(activityDataSet,activityLabelsTable)
        
        ## read subject data set
        filePath <- paste0(startFilePath, type, "/subject_", type, fileExtension)
        subjectDataSet <- read.table(filePath, header=FALSE, col.names = "subject")
        
        ## read main data set and add column names in the data set based on features
        filePath <- paste0(startFilePath, type, "/X_", type, fileExtension)
        humanActivityRecognitionDataSet <- read.table(filePath,
                                                      header=FALSE,
                                                      col.names = featuresTable$featureName,
                                                      check.names=FALSE)
        
        ## bind acitivity name into the main data set
        humanActivityRecognitionDataSet <- cbind(activityName = activityDataSetWithLabel$activityName , humanActivityRecognitionDataSet)
        
        ## bind subject code into the main data set
        humanActivityRecognitionDataSet <- cbind(subject = subjectDataSet$subject , humanActivityRecognitionDataSet)
        
    }
    
    ## read acticvity labels
    activityLabelsTable <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",
                                      header=FALSE,
                                      sep = "",
                                      col.names = c("activityCode","activityName"))
    
    ## convert to lowercase and replace special characters
    activityLabelsTable$activityName <- tolower(activityLabelsTable$activityName)
    activityLabelsTable$activityName <- gsub('_', ' ', activityLabelsTable$activityName)
    
    ## read features
    featuresTable <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",
                                header=FALSE,
                                sep = "",
                                col.names = c("featureIndex","featureName"))
    
    ## read and create the test and train data sets
    humanActivityRecognitionTestDataSet <- create_data_set("test")
    humanActivityRecognitionTrainDataSet <- create_data_set("train")

    ## merge the test and train data sets
    mergedTestAndTrainDataSets <- rbind(humanActivityRecognitionTestDataSet,
                                        humanActivityRecognitionTrainDataSet)
    
    ## tidy column names
    names(mergedTestAndTrainDataSets) <- tolower(names(mergedTestAndTrainDataSets))
    names(mergedTestAndTrainDataSets) <- gsub("[^[:alnum:]]","",names(mergedTestAndTrainDataSets))
    
    ## extract the mean and standard deviation for each measurement
    mergedMeanAndStdDevDataSet <- mergedTestAndTrainDataSets[,grepl("subject|activity|mean|std", names(mergedTestAndTrainDataSets))]
    
    ## create a new data by calculating the mean of the data based on activity and subject
    library(dplyr)
    meanBasedOnActivtyAndSubjectDataSet <- mergedMeanAndStdDevDataSet %>% group_by(subject, activityname) %>% summarise_all(mean)
    
    ## output to file the final data set containing the calculated data
    write.table(meanBasedOnActivtyAndSubjectDataSet, file = "meanBasedOnActivtyAndSubjectDataSet.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
    
    meanBasedOnActivtyAndSubjectDataSet
    
}
