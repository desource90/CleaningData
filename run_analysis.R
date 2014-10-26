#################################################################################
#     This script is meant to reproduce the entire process of downloading and   #   
# cleaning the raw UCI HAR dataset.                                             #    
#################################################################################

# Install and load all the packages used for the cleanup
install.packages("tidyr")
install.packages("dplyr")
install.packages("data.table")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(data.table)
library(stringr)

# Create directory and download and store the data.
if (!file.exists("data")) {
      dir.create("data")
}
setwd("data")

# If using a mac, "apple" should exist in your platform name, and you need to include
# the argument, method="curl", when downloading files from https. If not a mac,
# the argument should not be included or the function will return an error.
ifelse(grepl("apple",sessionInfo()$platform),
       download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Activity_Data.zip"
              ,method="curl"),
       download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Activity_Data.zip")
)

list.files()
unzip("Activity_Data.zip")
list.files()
setwd("UCI HAR Dataset/")

# Preview the data to decide on the appropriate column labels.
read.table("activity_labels.txt",nrow=5)
ActivityLabels <- data.table(read.table("activity_labels.txt",col.names=c("ActivityID","Activity")))

read.table("features.txt",nrow=5)
Features <- read.table("features.txt",col.names=c("FeatureID","Feature"))

setwd("test")
list.files()

Subject <- read.table("subject_test.txt",col.names="SubjectID")
Activity <- read.table("y_test.txt",col.names="ActivityID")
Test_Data <- tbl_df(read.table("X_test.txt"))

# Check the dimensions of the data to see how to join the various DFs.
dim(Test_Data)

# Set the column labels to the feature names.
setnames(Test_Data,as.character(Features[,2]))

# Looking at the number of rows of each DF, it seems like this is how the data are linked.
# Each measurement refers to a particular activity conducted by a subject.
Test_Data <- cbind(Subject,Activity,Test_Data)

# Move on to the train directory and its contents.
setwd("../train")
list.files()

# Use a similar process to consume the Train Data.
Subject <- read.table("subject_train.txt",col.names="SubjectID")
Activity <- read.table("y_train.txt",col.names="ActivityID")
Train_Data <- tbl_df(read.table("X_train.txt"))
dim(Train_Data)
setnames(Train_Data,as.character(Features[,2]))
Train_Data <- cbind(Subject,Activity,Train_Data)

# Combine the rows of the Test and Train data sets.
Merged_Data <- rbind(Test_Data,Train_Data) ; print(colnames(Merged_Data))

# Since we are only interested in the mean and std of each measurement, we should only select
# those columns. meanfrequency seems like an entirely different measurement altogether, so we
# can leave that out.
Merged_Data <- Merged_Data %>% select(SubjectID,ActivityID,contains("mean()"),contains("std()"))

# The previous attempt to select the columns of interest failed because of an error.
            ## "Error: found duplicated column name". ## 
# Let's investigate into the error to understand why it happened and fix it.

# Create and view a frequency table of all the column names. The dataset does indeed 
# have variables that are repeated multiple times.
Columns <- table(colnames(Merged_Data)); print(Columns)

# Isolate those duplicate variables and view their column names.
Duplicate_Columns <- row.names(subset(Columns,Columns>1)); print(Duplicate_Columns)

# The variables are all related to bandsEnergy, which is nothing of interest to us, 
# so we can remove all of them from our dataset by getting the column numbers.
Duplicate_Columns_Numbers <- Features[Features$Feature %in% Duplicate_Columns,1]

# With the column numbers, subset our original dataset to exclude 
# those duplicate column numbers.
Merged_Data <- Merged_Data[,-(Duplicate_Columns_Numbers)]

# The second attempt to select only the columns of interest. Merge the Activity
# labels data into our dataset as well.
Merged_Data <- Merged_Data %>%
      select(SubjectID,ActivityID,contains("mean()"),contains("std()")) %>%
      merge(ActivityLabels) %>%
      tbl_df %>%
      select(SubjectID,ActivityID,Activity,3:67) %>%
      arrange(SubjectID,ActivityID)
      

# Clean up the column names. The parentheses are illegal characters
# in column names and makes it difficult to reference those columns by their names.
# Therefore, remove those parentheses, as well as convert the dashes into underscores
# for aesthetic purposes.
colnames(Merged_Data) <- colnames(Merged_Data) %>%
      str_replace("\\(\\)","") %>%
      str_replace_all("-","_")

# It's unnecessary to recode the variable names since it's already descriptive and compact,
# and name fits nicely in the environment display. A quick reference to the
# codebook also allows us to understand the definition of each section of the variable name.

# Create summary data. Group by Subject and Activity to get the mean of each measurement by
# those indices.
Summary_Data <- Merged_Data %>%
      group_by(SubjectID,Activity) %>%
      summarise_each(funs(mean)) %>%
      arrange(SubjectID,ActivityID)

# Naming the variables of our new summary dataset so it's clear that we're taking the average
# of each measurement variable.
colnames(Summary_Data) <- colnames(Summary_Data) %>%
      str_replace("_mean","mean_of_means") %>%
      str_replace("_std","mean_of_std")
      