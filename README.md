CleaningData
============

# The RScript requires the following packages: stringr,dplyr,data.table

The run_analysis RScript downloads, unzips and cleans the raw UCI HAR dataset. After appending the correct columns into the test and train datasets, the two datasets are combined into one. The activity labels are appended as well. 

After we have a tidy data set, we are able to create a second tidy dataset that contains a summary of the measurements with the average of each measured variable for each activity and for each study participant.
