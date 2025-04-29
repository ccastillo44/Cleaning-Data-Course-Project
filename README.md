# Cleaning-Data-Course-Project
Cleaning Data Course Project
# This script processes the UCI HAR Dataset to create a tidy dataset as per project requirements.

# Load required packages
library(dplyr)
library(tidyr)

setwd("~/Cleaning-Data-Course-Project/UCI_HAR_Dataset/")

# Step 1: Merge the training and test sets
# Read training data
train_subjects <- read.table("train/subject_train.txt", col.names = "Subject")
train_activities <- read.table("train/y_train.txt", col.names = "Activity")
train_data <- read.table("train/X_train.txt")

# Read test data
test_subjects <- read.table("test/subject_test.txt", col.names = "Subject")
test_activities <- read.table("test/y_test.txt", col.names = "Activity")
test_data <- read.table("test/X_test.txt")

# Read feature names
features <- read.table("features.txt", col.names = c("Index", "FeatureName"))

# Assign feature names to columns of train and test data
colnames(train_data) <- features$FeatureName
colnames(test_data) <- features$FeatureName

# Combine subject, activity, and measurements
train_combined <- cbind(train_subjects, train_activities, train_data)
test_combined <- cbind(test_subjects, test_activities, test_data)

# Merge train and test datasets
full_data <- rbind(train_combined, test_combined)

# Step 2: Extract measurements on mean and standard deviation
# Select columns for Subject, Activity, and features containing "mean()" or "std()"
mean_std_columns <- grepl("Subject|Activity|mean\\(\\)|std\\(\\)", colnames(full_data))
data_mean_std <- full_data[, mean_std_columns]

# Step 3: Use descriptive activity names
# Read activity labels
activity_labels <- read.table("activity_labels.txt", col.names = c("ActivityID", "ActivityName"))

# Replace activity IDs with descriptive names
data_mean_std$Activity <- factor(data_mean_std$Activity, 
                                 levels = activity_labels$ActivityID, 
                                 labels = activity_labels$ActivityName)

# Step 4: Appropriately label the dataset with descriptive variable names
# Clean feature names: remove special characters, make consistent
# Clean column names: remove parentheses and replace hyphens with underscores
colnames(data_mean_std) <- gsub("\\(\\)", "", colnames(data_mean_std))  # Remove ()
colnames(data_mean_std) <- gsub("-", "_", colnames(data_mean_std))       # Replace - with _
colnames(data_mean_std) <- gsub("-", "_", colnames(data_mean_std))
colnames(data_mean_std) <- make.names(colnames(data_mean_std), unique = TRUE)

# Step 5: Create a tidy dataset with the average of each variable for each activity and subject
tidy_data <- data_mean_std %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean, na.rm = TRUE)

# Write the tidy dataset to a text file
write.table(tidy_data, "tidy_data.txt", row.names = FALSE, quote = FALSE)

# Optional: Save as CSV for easier reading
write.csv(tidy_data, "tidy_data.csv", row.names = FALSE)
