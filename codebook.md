# UCI HAR Data set information

This data set is a modified version of the "Human Activity Recognition using Smatphones" data set that can be found in the following link:  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The modification included:

1.  combining the train and test data sets

2.  replacing the variable names with more indicative ones

3.  extracting only std and mean columns of all the features

4.  calculating the mean of each feature grouped by subject and activity performed


The original data set is made of data of 30 subjects who perform 6 activities each, for whom 33 measures were evaluated multiple times. In this reduced version, the observations were replaced by the average of their means and standard deviations. After these modifications the result is a data set of 180 rows and 68 columns, where each column represents one of the following variables:

subject

        range from 1 to 30
        
activity

        6 types:
        
        1. walking
        2. walking_upstairs
        3. walking_downstairs
        4. sitting
        5. standing
        6. Laying

tBodyAcc_mean_X

tBodyAcc_mean_Y

tBodyAcc_mean_Z

tGravityAcc_mean_X

tGravityAcc_mean_Y

tGravityAcc_mean_Z

tBodyAccJerk_mean_X

tBodyAccJerk_mean_Y

tBodyAccJerk_mean_Z

tBodyGyro_mean_X

tBodyGyro_mean_Y

tBodyGyro_mean_Z

tBodyGyroJerk_mean_X

tBodyGyroJerk_mean_Y

tBodyGyroJerk_mean_Z

tBodyAccMag_mean

tGravityAccMag_mean

tBodyAccJerkMag_mean

tBodyGyroMag_mean

tBodyGyroJerkMag_mean

fBodyAcc_mean_X

fBodyAcc_mean_Y

fBodyAcc_mean_Z

fBodyAccJerk_mean_X

fBodyAccJerk_mean_Y

fBodyAccJerk_mean_Z

fBodyGyro_mean_X

fBodyGyro_mean_Y

fBodyGyro_mean_Z

fBodyAccMag_mean

fBodyBodyAccJerkMag_mean

fBodyBodyGyroMag_mean

fBodyBodyGyroJerkMag_mean

tBodyAcc_std_X

tBodyAcc_std_Y

tBodyAcc_std_Z

tGravityAcc_std_X

tGravityAcc_std_Y

tGravityAcc_std_Z

tBodyAccJerk_std_X

tBodyAccJerk_std_Y

tBodyAccJerk_std_Z

tBodyGyro_std_X

tBodyGyro_std_Y

tBodyGyro_std_Z

tBodyGyroJerk_std_X

tBodyGyroJerk_std_Y

tBodyGyroJerk_std_Z

tBodyAccMag_std

tGravityAccMag_std

tBodyAccJerkMag_std

tBodyGyroMag_std

tBodyGyroJerkMag_std

fBodyAcc_std_X

fBodyAcc_std_Y

fBodyAcc_std_Z

fBodyAccJerk_std_X

fBodyAccJerk_std_Y

fBodyAccJerk_std_Z

fBodyGyro_std_X

fBodyGyro_std_Y

fBodyGyro_std_Z

fBodyAccMag_std

fBodyBodyAccJerkMag_std

fBodyBodyGyroMag_std

fBodyBodyGyroJerkMag_std
