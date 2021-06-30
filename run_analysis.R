library(dplyr); library(stringr); library(mgsub)

##reads train and test datasets and combines them
data_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
data_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
data <- rbind(data_train, data_test)

##reads train and test set of activity labels and combines them, then renames the variable
labels_train <- read.delim("UCI HAR Dataset/train/y_train.txt", header = FALSE)
labels_test <- read.delim("UCI HAR Dataset/test/y_test.txt", header = FALSE)
labels <- rbind(labels_train, labels_test)
labels <- rename(labels, activity = V1)

##reads train and test set of subject ids and combines them, then renames the variable
subject_train <- read.delim("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_test <- read.delim("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subject_id <- rbind(subject_train, subject_test)
subject_id <- rename(subject_id, subject_id = V1)

##reads the headers, removes line numbers and extra spaces from the headers, 
##adds headers to the data table
headers <- read.delim("UCI HAR Dataset/features.txt", header = FALSE)
headers_char <- headers[, 1]
headers_char <- gsub('[[:digit:]]+', "", headers_char)
headers_char <- gsub(" ", "", headers_char)
colnames(data) <- headers_char

##binds the columns of subject ids, activity labels and the data table
data <- cbind(subject_id, labels, data)

##extracts the headers, sets all headers to lower case, identifies columns
##containing "mean" or "std", then subsets relevant columns
headers_data <- names(data)
headers_formatted <- tolower(headers_data)
keep_cols <- grep("mean|std", headers_formatted)
keep_cols <- append(c(1, 2), keep_cols)
data <- data[, keep_cols]

##reads and tides activities, then adds character strings to the data table
activities <- read.delim("C:/Users/szaka/Desktop/Dokumentumok/R/UCI HAR Dataset/activity_labels.txt", header = FALSE)
activities <- as.character(activities[, 1])
activities <- gsub('[[:digit:]]+', "", activities)
activities <- gsub(" ", "", activities)
data$activity <- mgsub(data$activity, c(1:6), activities)

##creates a data set with the average of each variable for each activity and each subject,
##then writes the output into a txt. file
data_subset <- group_by(data, subject_id, activity)
summary <- summarize_at(data_subset, vars(, 3:86), funs(mean(., na.rm=TRUE)))
write.table(summary, "summary.txt", row.names = FALSE)