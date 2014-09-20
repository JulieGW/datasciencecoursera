#This code will download the SAMSUNG Gallaxy S Smart phone sensor data (accelerometer and
# gryoscope) from an experiment montioring this for different person positions.  The data is 
# originally set up to do a machine learning test to match the data to the persons position
# with the phone.  For that reason the person's activity while using the phone, ie walking or 
# lying down is preserved in the first column.  This simplified set has the mean and standard 
# deviation data only and does not process the original data from which these values are drawn.

#This work is being done for the Class Assignement for "Getting & Cleaning Data" by Coursera
print("Downloading the data")
# Step 1 is to download the file.  First make sure you are in the working directory you want before
# loading this script.  
  if(!file.exists("data")){dir.create("data")}
  fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/HAR.zip",method="curl")

# a download date is collected at the same time
  downloaddate<-date()
# unzip file manually or using the following OSx (Mac) command 
  unzip("./data/HAR.zip")

print("Reading the data into R")
# load in the files needed to variables for manipulation
  xtest<-read.table("./data/UCI HAR Dataset/test/X_test.txt",header=F,stringsAsFactors=F,fill=TRUE)
  ytest<-read.table("./data/UCI HAR Dataset/test/y_test.txt",header=F,stringsAsFactors=F,fill=TRUE)
  xtrain<-read.table("./data/UCI HAR Dataset/train/X_train.txt",header=F,stringsAsFactors=F,fill=T)
  ytrain<-read.table("./data/UCI HAR Dataset/train/y_train.txt",header=F,stringsAsFactors=F,fill=T)
  subjectstest<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  subjectstrain<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")

########################################################################
# Since test and train are sections of the same data set, recreate that by placeing test on top 
# of train for the all sections, the data (X), the answers (activities) (y) and subjects.  
# This represents most of step 1 in the instructions
# These two will be combined into one set further down, completing step 1.  

print("Tidying up the data")

  HARdata<-rbind(xtest,xtrain)
  HARans<-rbind(ytest,ytrain)
  subjects<-rbind(subjectstest,subjectstrain)

##################################################
# There are 561 columns of data of which we only want those which reference mean or 
# standard deviation.  The columns need to be labeled, then the data sub-setted.
# This is step 2 of the instructions

  #Now place the names of the columns on each column for the data set
    cnames<-read.table("./data/UCI HAR Dataset/features.txt")
    colnames(HARdata)<-cnames[,2]
  
#using these names select out only the columns with mean or standard deviation in the title
# it is easier to use the dplyr package. The plyr package is used for summary at the end of this 
# file, however to avoid error messages, it loads first. The install command is included if
# needed. 
#un-comment the following two lines as needed.
  
#install.packages("plyr")
  library(plyr)
 #install.packages("dplyr")
  library(dplyr)

# Subset the data into a shorter width set which has only the columns with mean or standard
# deviation in the name. 
  short<-select(HARdata,contains("mean"),contains("std"))

#############################################################################
# Steps 3 and 4 
# Making nice names

# Appropriately label the the columns in the data set with better variable names.  There are
  #86 of these, so the labels were modified to make them more readable, but not individually 
  #name them.  The label meaning is as follows:
  tempnames<-names(short)
  tempnames<-gsub("()","",tempnames,fixed=TRUE)
  tempnames<-gsub("-"," ",tempnames,fixed=TRUE)
  names(short)<-tempnames

# The column names now have the following convention: t or f is whether the data is in the time
# domain or frequency domain.  The data comes from the output of accelerometers (Acc) or gyroscopes
# (Gyro), and the signal has been separated into the gravity component (Gravity) or that which
# came from the body of the person it was attached to (Body).  These forces are given in the X,
# Y, and Z directions.  tBodyAcc mean X is the time domain mean for the body part of the 
# accelerometer signal in the X direction.

# Units:  All signals have been normalized, so they are unitless

# The activity lables are given in numerical code in the "answers" file.  These are then converted
# into readable names for the tidy data set.  

  alabels<-read.table("./data/UCI HAR Dataset/activity_labels.txt")

  # Put the right activity label into column 2, label the two columns
  for (i in 1:nrow(HARans)){
    HARans[i,2]<-alabels[(HARans[i,1]),2]
  }
  names(HARans)<-c("original code","activity")

  # modify that activity lable to make it a little nicer by getting rid of the "_"
  X<-as.character(HARans[,2])
  X<-gsub("_"," ",X,fixed=TRUE)
  HARans[,2]<-X

  #Give the subjects a name for their column
  names(subjects)<-"subject"

  HARactivity<-HARans[,2] # Gives the option of keeping the activities as codes or just names
#######################################

  print("Creating final dataframe for the first 4 steps")
#Combine the answers to the data into one data set, completeing step 1-4

#Note: if you plan to do machine learning on this file, use the commented out final combine
# as that will give a column with the original activity codes, which are easier to use in 
# machine learning.  If you just want to look at the data, use the file as written.

# HARfinalmachinelearing<-cbind(HARans,subjects,short)# this is the machine learning version

  HARfinal<-cbind(HARactivity,subjects,short) #this is the nice to look at version
  #####################################


# create a separate table that is each activity and the means of all of the features 
  summary<-ddply(HARfinal,.(HARactivity),numcolwise(mean))
  
  #######################################  

# Create a .txt file that contains the summary table 

write.table(summary,file="HARdataSummary.txt",row.names=FALSE)