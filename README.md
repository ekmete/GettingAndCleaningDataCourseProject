Coursera Getting And Cleaning Data Course Project
=================================================

Content of Repository
---------------------

This repo contains four different files.

-   `README.md`: provides information about repository and how the tidy data is generated.

-   `tidydata.txt`: contains the tidy data set.

-   `CodeBook.md`: provides information about content of the data set.

-   `run_analysis.R`, the R script that was used to create the data set.

How does R script `run_analysis.R` work?
----------------------------------------

`run_analysis.R` produces the tidy data set by implementing the following steps:

-   Download and unzip source data if it doesn't exist.

-   Read data.

-   Merge the training and the test sets to create one data set.

-   Extract only the measurements on the mean and standard deviation for each measurement.

-   Use descriptive activity names to name the activities in the data set.

-   Appropriately label the data set with descriptive variable names.

-   Create a second, independent tidy set with the average of each variable for each activity and each subject.

-   Write the data set to the `tidydata.txt` file.
