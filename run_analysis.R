# 1. Merges the training and the test sets to create one data set.
setwd("C:/Cursos-Online/Getting and Cleaning Data/files")
x_train <- read.table("X_train.txt")
x_test <- read.table("X_test.txt")
subject_train <- read.table("subject_train.txt")
subject_test <- read.table("subject_test.txt")
y_train <- read.table("y_train.txt")
y_test <- read.table("y_test.txt")
x <- rbind(x_train,x_test)
y <- rbind(y_train,y_test)
subject <- rbind(subject_train,subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#    Final file: xok
features <- read.table("features.txt")
featuresok <- grepl("-mean|-std",features[,2])
names(x) <- features[,2] # 4. Appropriately labels the data set with descriptive variable names
xok <- x[featuresok]

# 3. Uses descriptive activity names to name the activities in the data set
# add activity number
activity <- read.table("activity_labels.txt")
names(activity) <- c("Number_Activity","Activity")
names(y) <- c("Number_Activity")
# for join
install.packages("plyr")
library(plyr)
activityok <- merge(y,activity,by.x="Number_Activity",by.y="Number_Activity",type=inner,all=TRUE)
xok <- cbind(activityok$Activity,xok)
names(xok) [1] <- "Activity" 
# add subject
names(subject) <- c("Subject")
xok <- cbind(subject,xok)

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
install.packages("dplyr")
library(dplyr)
avg_by_act_sub <- xok %>% 
  group_by(Subject,Activity) %>% 
  summarise_each(funs(mean))

library(reshape2)
avg_by_act_subMelt <- melt(xok,id=c("Subject","Activity"))
avg_by_act_sub <- dcast(avg_by_act_subMelt,Subject + Activity  ~ variable, fun=mean)