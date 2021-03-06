Coursera Getting And Cleaning Data Course Project Codebook
==========================================================

This codebook explains the dataset named "tidydata.txt" which is located in this repo.

Data
----

tidydata.txt contains space-separated data. First column contains variable names and other rows contain values.

Variables
---------

### Identifiers

-   `subject`

    Identifies subject, integer valued, ranged from 1 to 30.

-   `activity`

    There are 6 types of activities. Each value defines the activity of subjects during measurements.

    -   `WALKING`

    -   `WALKING_UPSTAIRS`

    -   `WALKING_DOWNSTAIRS`

    -   `SITTING`

    -   `STANDING`

    -   `LAYING`

### Measurements

There are 79 different measurements, which is the average of each variable for each activity and each subject. There are 6 values for each subject, resulting 180 rows in total. Each measurement has numeric value.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise.

Similarly, the acceleration signal was then separated into body and gravity acceleration signals using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals. Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm.

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern: 'XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

All measurements show mean or standard deviation of signals.

-   `timeDomainBodyAccelerometerMeanX`
-   `timeDomainBodyAccelerometerMeanY`
-   `timeDomainBodyAccelerometerMeanZ`

-   `timeDomainBodyAccelerometerStandardDeviationX`
-   `timeDomainBodyAccelerometerStandardDeviationY`
-   `timeDomainBodyAccelerometerStandardDeviationZ`

-   `timeDomainGravityAccelerometerMeanX`
-   `timeDomainGravityAccelerometerMeanY`
-   `timeDomainGravityAccelerometerMeanZ`

-   `timeDomainGravityAccelerometerStandardDeviationX`
-   `timeDomainGravityAccelerometerStandardDeviationY`
-   `timeDomainGravityAccelerometerStandardDeviationZ`

-   `timeDomainBodyAccelerometerJerkMeanX`
-   `timeDomainBodyAccelerometerJerkMeanY`
-   `timeDomainBodyAccelerometerJerkMeanZ`

-   `timeDomainBodyAccelerometerJerkStandardDeviationX`
-   `timeDomainBodyAccelerometerJerkStandardDeviationY`
-   `timeDomainBodyAccelerometerJerkStandardDeviationZ`

-   `timeDomainBodyGyroscopeMeanX`
-   `timeDomainBodyGyroscopeMeanY`
-   `timeDomainBodyGyroscopeMeanZ`

-   `timeDomainBodyGyroscopeStandardDeviationX`
-   `timeDomainBodyGyroscopeStandardDeviationY`
-   `timeDomainBodyGyroscopeStandardDeviationZ`

-   `timeDomainBodyGyroscopeJerkMeanX`
-   `timeDomainBodyGyroscopeJerkMeanY`
-   `timeDomainBodyGyroscopeJerkMeanZ`

-   `timeDomainBodyGyroscopeJerkStandardDeviationX`
-   `timeDomainBodyGyroscopeJerkStandardDeviationY`
-   `timeDomainBodyGyroscopeJerkStandardDeviationZ`

-   `timeDomainBodyAccelerometerMagnitudeMean`
-   `timeDomainBodyAccelerometerMagnitudeStandardDeviation`

-   `timeDomainGravityAccelerometerMagnitudeMean`
-   `timeDomainGravityAccelerometerMagnitudeStandardDeviation`

-   `timeDomainBodyAccelerometerJerkMagnitudeMean`
-   `timeDomainBodyAccelerometerJerkMagnitudeStandardDeviation`

-   `timeDomainBodyGyroscopeMagnitudeMean`
-   `timeDomainBodyGyroscopeMagnitudeStandardDeviation`

-   `timeDomainBodyGyroscopeJerkMagnitudeMean`
-   `timeDomainBodyGyroscopeJerkMagnitudeStandardDeviation`

-   `frequencyDomainBodyAccelerometerMeanX`
-   `frequencyDomainBodyAccelerometerMeanY`
-   `frequencyDomainBodyAccelerometerMeanZ`

-   `frequencyDomainBodyAccelerometerStandardDeviationX`
-   `frequencyDomainBodyAccelerometerStandardDeviationY`
-   `frequencyDomainBodyAccelerometerStandardDeviationZ`

-   `frequencyDomainBodyAccelerometerMeanFrequencyX`
-   `frequencyDomainBodyAccelerometerMeanFrequencyY`
-   `frequencyDomainBodyAccelerometerMeanFrequencyZ`

-   `frequencyDomainBodyAccelerometerJerkMeanX`
-   `frequencyDomainBodyAccelerometerJerkMeanY`
-   `frequencyDomainBodyAccelerometerJerkMeanZ`

-   `frequencyDomainBodyAccelerometerJerkStandardDeviationX`
-   `frequencyDomainBodyAccelerometerJerkStandardDeviationY`
-   `frequencyDomainBodyAccelerometerJerkStandardDeviationZ`

-   `frequencyDomainBodyAccelerometerJerkMeanFrequencyX`
-   `frequencyDomainBodyAccelerometerJerkMeanFrequencyY`
-   `frequencyDomainBodyAccelerometerJerkMeanFrequencyZ`

-   `frequencyDomainBodyGyroscopeMeanX`
-   `frequencyDomainBodyGyroscopeMeanY`
-   `frequencyDomainBodyGyroscopeMeanZ`

-   `frequencyDomainBodyGyroscopeStandardDeviationX`
-   `frequencyDomainBodyGyroscopeStandardDeviationY`
-   `frequencyDomainBodyGyroscopeStandardDeviationZ`

-   `frequencyDomainBodyGyroscopeMeanFrequencyX`
-   `frequencyDomainBodyGyroscopeMeanFrequencyY`
-   `frequencyDomainBodyGyroscopeMeanFrequencyZ`

-   `frequencyDomainBodyAccelerometerMagnitudeMean`
-   `frequencyDomainBodyAccelerometerMagnitudeStandardDeviation`
-   `frequencyDomainBodyAccelerometerMagnitudeMeanFrequency`

-   `frequencyDomainBodyAccelerometerJerkMagnitudeMean`
-   `frequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation`
-   `frequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency`

-   `frequencyDomainBodyGyroscopeMagnitudeMean`
-   `frequencyDomainBodyGyroscopeMagnitudeStandardDeviation`
-   `frequencyDomainBodyGyroscopeMagnitudeMeanFrequency`

-   `frequencyDomainBodyGyroscopeJerkMagnitudeMean`
-   `frequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation`
-   `frequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency`

Transformation
--------------

Original data is obtained from the following link:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

Following transformations are applied to the original data:

1.  Original data consists of two separated data sets, training and test sets. These are merged and one data set is created.

2.  Only the measurements on the mean and standard deviation for each measurement are extracted.

3.  Descriptive activities in the data set are named using descriptive activity names.

4.  Data set is appropriately labeled using descriptive variable names.

    -   Special characters (i.e. `.` , `..`) were removed
    -   The initial `f` and `t` are replaced with `frequencyDomain` and `timeDomain` respectively.
    -   `Acc`, `Gyro`, `Mag`, `Freq`, `mean`, and `std` are replaced with `Accelerometer`, `Gyroscope`, `Magnitude`, `Frequency`, `Mean`, and `StandardDeviation` respectively.
    -   Replaced`BodyBody` with `Body`.

5.  From the data set in previous step, a second, independent tidy data set with the average of each variable for each activity and each subject is created.
