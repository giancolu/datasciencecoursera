###################################################################################
#### NOTE: extracted UCI HAR Dataset (source data) is in the working directory ####
###################################################################################
#### CREATING UCI-Samsung Dataset 1 ####
########################################
## Crate a dataset of subjects from "subjet_test" and "subject_train" datasets
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject.id")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject.id")
## Create a dataset of 561 features from "features.txt" and rename them to lower case  
features <- read.table("UCI HAR Dataset/features.txt")
feat1 <- tolower(features[,2])
## Create a dataset of 6 activity labels from "activity_labels.txt" and chage them as factor (6 levels)
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
f.activity <- as.character(activity[,2])
## Create complete test and train datasets
x.test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feat1) # features data
y.test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity") # coded activity
test.db <- cbind(subject.test,y.test,x.test)
x.train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feat1) # features data 
y.train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity") # coded activity
train.db <- cbind(subject.train,y.train,x.train)
## Merge train and test dataset
db <- rbind(train.db,test.db)
## Replace coded activity with their labels according to 6 levels
db$activity <- as.factor(db$activity)
levels(db$activity) <- f.activity
## Change as factor subject_id (30 levels)
db$subject.id <- as.factor(db$subject.id)
## Select features that represent only mean and std and create dataset "okdb"
smean <- "mean...[xyz]"
sstd <- "std...[xyz]"
column <- colnames(db)
okmean <- grepl(smean,column)
okstd <- grepl(sstd,column)
dbmean <- db[okmean]
dbstd <- db[okstd]
okdb <- cbind(db[,1:2],dbmean,dbstd)
## Optimize columns name according to R standard
newcol <- sub("...",".",(names(okdb)),fixed=T)
names(okdb) <- newcol
## Export "Samsung_dataset_1.csv"
write.csv(okdb, file = "samsung_dataset_1.csv")
########################################
#### CREATING UCI-Samsung Dataset 2 ####
########################################
## Aggregate data of dataset 1 to show the average of each variable for each activity and each subject
okdb1 <- aggregate(okdb[,3:50],list(subject = okdb$subject.id, activity = okdb$activity),mean)
## change columns name according to the new created variable
col <- names(okdb1[,3:50])
col1 <- paste("avg",col, sep = ".")
names(okdb1) <- c("subject","activity",col1)
## Export "Samsung_dataset_2"
write.csv(okdb1,"samsung_dataset_2.csv")
