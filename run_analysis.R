# This script merges all files into one data set, subsetting to extract only the measurements on the mean and standard deviation for each measurement.

library(reshape2)

processFiles <- function(fileType) {
    
    #get the activity ID
    path <- file.path(fileType, paste0("y_", fileType, ".txt"))
    y_data <- read.table(path, header=F, col.names=c("ActivityID"))
    
    # get the subject ID
    path <- file.path(fileType, paste0("subject_", fileType, ".txt"))
    subject <- read.table(path, header=F, col.names=c("SubjectID"))
    
    # get the column names from the features files
    variableNames <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the file
    path <- file.path(fileType, paste0("X_", fileType, ".txt"))
    dataset <- read.table(path, header=F, col.names=variableNames$MeasureName)
    
    # get only the mean and std related columns
    subset_variableNames <- grep(".*mean\\(\\)|.*std\\(\\)", variableNames$MeasureName)
    
    # get rid of not needed columns
    dataset <- dataset[,subset_variableNames]
    
    # add the activity id and subject id  as new variables
    dataset$ActivityID <- y_data$ActivityID
    dataset$SubjectID <- subject$SubjectID
    
    dataset
}

# merge the training and test data 
mergeData <- function() {
    
    # Merge both datasets into one
    # Improve the column headers to look better
    dataset <- rbind(processFiles("test"),processFiles("train"))
    cnames <- colnames(dataset)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(dataset) <- cnames
    
    #add activity names as another column
    activities <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activities$ActivityName <- as.factor(activities$ActivityName)
    mergedData <- merge(dataset, activities)
    mergedData
}

# Generate a tydy dataset and save it to a file
generateTidyDataset <- function(fileName) {
    
    # melt
    ids = c("ActivityID", "ActivityName", "SubjectID")
    measures = setdiff(colnames(mergeData()), ids)
    melted <- melt(mergeData(), id=ids, measure.vars=measures)
    
    # recast 
    tidy <- dcast(melted, ActivityName + SubjectID ~ variable, mean)    
    
    #save to a file
    write.table(tidy, fileName, row.names=FALSE)
}


generateTidyDataset("tidy.txt")
print("Ok!")