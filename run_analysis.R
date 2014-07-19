
library(data.table)

feat <- fread("UCI HAR Dataset/features.txt")$V2
act <- fread("UCI HAR Dataset/activity_labels.txt")

setnames(act, "V2", "activity")

test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, colClasses=c("numeric"))
testy <- read.table("UCI HAR Dataset/test/Y_test.txt", header = FALSE, colClasses=c("factor"))
tests <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, colClasses=c("factor"))
names(tests) <- c("subject")
train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, colClasses=c("numeric"))
trainy <- read.table("UCI HAR Dataset/train/Y_train.txt", header = FALSE, colClasses=c("factor"))
trains <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, colClasses=c("factor"))
names(trains) <- c("subject")

colnames(test) <- feat
colnames(train) <- feat

dtest <- cbind(tests, test, testy)
dtrain <- cbind(trains, train, trainy)

d <- rbind(dtest, dtrain)

cols <- names(d)
cols <- grep("(-(mean|std)\\(\\)|V1|subject)", cols, value=T)

d <- d[, cols]

dat <- merge(d, act)
dat$V1 <- NULL

tidy <- aggregate(. ~ activity + subject, data=dat, mean)

str(tidy)
summary(tidy)
head(tidy)

write.table(tidy, file="tidy.txt")


library(ggplot2)
dat$activity <- as.factor(dat$activity)
qplot(dat[,3], dat$subject, col=dat$activity)
