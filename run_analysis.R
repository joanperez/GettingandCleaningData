##################################################################
######### Author: Joan Manuel Perez <joan.perez.cardona@gmail.com>
######### Getting and Cleaning Data Course Project
######### Project of Getting and Cleaning Data course on Coursera, Aug 2016 edition.
##################################################################

require(plyr)

##################################################################
######### 1. Download the file and put the file in the data folder
##################################################################

if(!file.exists("./UCIHARDataset")){dir.create("./UCIHARDataset")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./UCIHARDataset/Dataset.zip",method="curl")

##################################################################
######### 2.Unzip the file
##################################################################

unzip(zipfile="./UCIHARDataset/Dataset.zip",exdir="./UCIHARDataset")

##################################################################
######### 3.unzipped files are in the folder "UCIHARDataset". Get the list of the files
##################################################################

path_rf <- file.path("./UCIHARDataset" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

##################################################################
######### 4.Directories and files
##################################################################

uci_hard_dir <- "./UCIHARDataset/UCI HAR Dataset"
feature_file <- paste(uci_hard_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")

##################################################################
######### 5.Load raw data
##################################################################

features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

##################################################################
######### 6.Merges the training and the test sets to create one data set.
##################################################################

# Binding sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

############################################################################################
######### 7. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

###########################################################################
######### 8. Uses descriptive activity names to name the activities in the data set
###########################################################################

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]

##############################################################
######### 9. Appropriately labels the data set with descriptive names.
##############################################################

# Remove parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
# Make syntactically valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))
# Make clearer names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))

######################################################################################################################
######### 10. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################

sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "tidydata_sensor_avg_by_act_sub.txt")