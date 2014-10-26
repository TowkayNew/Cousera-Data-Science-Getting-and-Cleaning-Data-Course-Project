# File Path
filepath <- "UCI HAR Dataset"

if(!file.exists(filepath)) {
    stop("Please ensure UCI HAR Dataset exists in your working directory!")
}

# Load or Install package 
if (!require("reshape2")) {
    install.packages("reshape2")
    library(reshape2)
}

message("Please wait... Getting and tidying dataset in progress...")

# File Path & Column Names
filepath <- paste(filepath, "/", sep="")
activity_colnames <- c("activityId", "activityLabel")
subject_colnames <- c("subjectId")

# Features extraction for X dataset
features <- read.table("UCI HAR Dataset/features.txt")[,2]
features_markup <- grepl("mean\\(\\)|std\\(\\)", features)
features <- features[features_markup]

# Activity extraction for Y dataset
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(activity_labels) <- activity_colnames

# Function to combine X, Y and Subject datasets
DataCombine <- function(postfix) {
    # Get Subject dataset
    subject <- read.table(paste(filepath, postfix, "/subject_", postfix, ".txt", sep=""))
    colnames(subject) <- subject_colnames[1]
    
    # Get Y dataset
    y <- read.table(paste(filepath, postfix, "/y_", postfix, ".txt", sep=""))
    colnames(y) <- activity_colnames[1]
    y <- merge(y, activity_labels)
    
    # Get X dataset
    x <- read.table(paste(filepath, postfix, "/X_", postfix, ".txt", sep=""))[,features_markup]
    colnames(x) <- features
    
    # Combine Subject, Y and X datasets
    data_combined <- cbind(subject, y[,activity_colnames[2]], x)
    colnames(data_combined)[2] <- activity_colnames[2]
    
    return(data_combined)
}

# Retrieve & combine test and train datasets
data <- rbind(DataCombine("test"), DataCombine("train"))

# Averaging of each variable for each activity and each subject.
data_melted <- melt(data, id=c("subjectId", "activityLabel"))
data_averaged <- dcast(data_melted, activityLabel + subjectId ~ variable, mean)

# Export tidy data
write.table(data_averaged, file="./tidy_data.txt", row.name=FALSE)
message("Tidy Data successfully created and exported to your working directory.")