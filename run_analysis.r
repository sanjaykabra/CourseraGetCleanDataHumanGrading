
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()


#Get the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datafile <- "SamsungData.zip"

#download only if not already downloaded. save network download time and bandwidth

if (!file.exists(datafile))
download.file(url, file.path(path, datafile))


#Unzip downloaded file
unzip(datafile)


pathIn <- file.path(path, "UCI HAR Dataset")
#list.files(pathIn, recursive=TRUE)

#See the README.txt file in r path for detailed information on the dataset.

#For the purposes of this project, the files in the Inertial Signals folders are not used.

#Read the files

#Read the subject files.

dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))
#Read the activity files. For some reason, these are called label files in the README.txt documentation.

dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

#Read the data files. 
fileToDataTable <- function (f) {
df <- read.table(f)
dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))

#Merge the training and the test sets

#Concatenate the data tables.

dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)
#Merge columns.

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
#Set key.

setkey(dt, subject, activityNum)

#Extract only the mean and standard deviation

#Read the features.txt file. This tells which variables in dt are 
#measurements for the mean and standard deviation.

dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

#Subset only measurements for the mean and standard deviation.
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

#Convert the column numbers to a vector of variable names matching columns in dt.
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

#Subset these variables using variable names.
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

#Use descriptive activity names
#Read activity_labels.txt file. add descriptive names to the activities.

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#Label with descriptive activity names
#Merge activity labels.
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

#Add activityName as a key.
setkey(dt, subject, activityNum, activityName)

#Melt the data table to reshape it from a short and wide format to a tall and narrow format.
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

#Merge activity name.
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

#Create a new variable, activity that is equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#Seperate features from featureName using the helper function grepthis.
grepthis <- function (regex) {
grepl(regex, dt$feature)
}

# Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

# Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

# Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

#Tidy up the data

#Final dataset with average of each variable of subject and activity
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]


#Refer to codebook for details of variables
str(dtTidy)

#List the key variables in the data table
key(dtTidy)

#Show a few rows of the dataset
dtTidy

#Summary of variables
summary(dtTidy)

#List all possible combinations of features
dtTidy[, .N, by=c(names(dtTidy)[grep("^feat", names(dtTidy))])]

#Save to file called HumanActivityusingSmartPhonesDataSet

f <- file.path(path, "HumanActivityusingSmartPhonesDataSet.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)
