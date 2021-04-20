library(data.table)
library(dplyr)


# Load all required data
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
subj_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
subj_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
features <- read.table('./UCI HAR Dataset/features.txt')

# Assign feature names to variables
names(X_train) <- features[[2]]
names(X_test) <- features[[2]]

# Assign subject and activity indicators to each observation
train <- cbind(subj_train, y_train, X_train)
test <- cbind(subj_test, y_test, X_test)

# Combine train and test data sets and give proper names to identification variables
data <- rbind(train, test)
names(data)[1] <- "subject"
names(data)[2] <- "activity"

# Extract mean and std data of features
means <- data[contains("mean()", vars = names(data))]
stds <- data[contains("std()", vars = names(data))]

# Combine identification variables with mean and std of features
data <- cbind(subject = data$subject, activity = data$activity, means, stds)

# Rename activity labels
activity_labels <- list("1"='walking',
                        "2"='walking_upstairs',
                        "3"='walking_downstairs',
                        "4"='sitting',
                        "5"='standing',
                        "6"='laying')
data$activity <- sapply(data$activity, function(x) x <- activity_labels[[x]])

# Rename features
names(data)[grep("-(mean|std).*", names(data))] <- sapply(names(data)[grep("-(mean|std).*", names(data))],
                                                              function(x) {
                                                                      
                                                                      x <- sub("[-]mean[(][)][-]", "_mean_", x)
                                                                      x <- sub("[-]mean[(][)]", "_mean", x)
                                                                      x <- sub("[-]std[(][)][-]", "_std_", x)
                                                                      x <- sub("[-]std[(][)]", "_std", x)
                                                              })

# Prepare tidy dataset
tidydata <- aggregate(data[, -c(1, 2)], list(data$subject, data$activity), mean)
names(tidydata)[1:2] <- c("subject", "activity")
tidydata <- arrange(tidydata, subject, activity)
write.csv(tidydata, file = 'tidy_UCI_HAR_data.csv')
