Getting and Cleaning Data Course Project
========================================================

The data is given in the subdirectory "UCI HAR Dataset/", divided
in test and traning sets. Furthermore, separate text files hold the feature matrix X<sub>ij</sub>, the numeric response vector Y<sub>i</sub> (the activities) and a table with the subject numbers, all without headers, i.e. column names.
The feature column names are given in features.txt and used unchanged. Names for the activities are found in activity_labels.txt, these will replace the numeric values. 


```r
library(data.table)

feat <- fread("UCI HAR Dataset/features.txt")$V2
act <- fread("UCI HAR Dataset/activity_labels.txt")

setnames(act, "V2", "activity")
```

For the time being test and training data ist read in separate data.tables.


```r
test <- read.table("UCI HAR Dataset/test/X_test.txt", header=F, colClasses=c("numeric"))
testy <- read.table("UCI HAR Dataset/test/Y_test.txt", header=F, colClasses=c("factor"))
tests <- read.table("UCI HAR Dataset/test/subject_test.txt", header=F, colClasses=c("factor"))
names(tests) <- c("subject")

train <- read.table("UCI HAR Dataset/train/X_train.txt", header=F, colClasses=c("numeric"))
trainy <- read.table("UCI HAR Dataset/train/Y_train.txt", header=F, colClasses=c("factor"))
trains <- read.table("UCI HAR Dataset/train/subject_train.txt", header=F, colClasses=c("factor"))
names(trains) <- c("subject")
```

The subject vector columns have been named with "subject" for both test and train dataset. The response vector columns keep their temporary default "V1" name to join later on the activity names.
The features now get their names and all tables are bind together to form one data set:


```r
colnames(test) <- feat
colnames(train) <- feat

dtest <- cbind(tests, test, testy)
dtrain <- cbind(trains, train, trainy)

d <- rbind(dtest, dtrain)
```

Now we use the column names to filter the desired mean() and std() measurements.


```r
cols <- names(d)
cols <- grep("(-(mean|std)\\(\\)|V1|subject)", cols, value=T)

d <- d[, cols]
```

Finally the activity numbers are replaced with their corresponding names.



```r
dat <- merge(d, act)
dat$V1 <- NULL
```

Now is time to aggregate the tidy data set with the average of each variable for each activity and each subject ...


```r
tidy <- aggregate(. ~ activity + subject, data=dat, mean)

str(tidy)
```

```
## 'data.frame':	180 obs. of  68 variables:
##  $ activity                   : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ subject                    : Factor w/ 30 levels "10","12","13",..: 1 1 1 1 1 1 2 2 2 2 ...
##  $ tBodyAcc-mean()-X          : num  0.28 0.271 0.277 0.279 0.29 ...
##  $ tBodyAcc-mean()-Y          : num  -0.0243 -0.015 -0.0155 -0.017 -0.02 ...
##  $ tBodyAcc-mean()-Z          : num  -0.117 -0.104 -0.108 -0.109 -0.111 ...
##  $ tBodyAcc-std()-X           : num  -0.968 -0.983 -0.978 -0.179 0.296 ...
##  $ tBodyAcc-std()-Y           : num  -0.94645 -0.91798 -0.91956 -0.02274 0.00408 ...
##  $ tBodyAcc-std()-Z           : num  -0.959 -0.968 -0.941 -0.396 -0.184 ...
##  $ tGravityAcc-mean()-X       : num  -0.453 0.792 0.954 0.963 0.94 ...
##  $ tGravityAcc-mean()-Y       : num  -0.1393 -0.0413 -0.0432 -0.0838 -0.0646 ...
##  $ tGravityAcc-mean()-Z       : num  -0.0311 0.2025 0.0347 0.0549 0.0419 ...
##  $ tGravityAcc-std()-X        : num  -0.955 -0.973 -0.991 -0.974 -0.939 ...
##  $ tGravityAcc-std()-Y        : num  -0.967 -0.936 -0.975 -0.971 -0.937 ...
##  $ tGravityAcc-std()-Z        : num  -0.963 -0.967 -0.96 -0.958 -0.902 ...
##  $ tBodyAccJerk-mean()-X      : num  0.0738 0.0775 0.081 0.0858 0.101 ...
##  $ tBodyAccJerk-mean()-Y      : num  0.0157 0.00898 0.0119 0.00408 0.01078 ...
##  $ tBodyAccJerk-mean()-Z      : num  0.007167 -0.004996 -0.004858 -0.016295 0.000153 ...
##  $ tBodyAccJerk-std()-X       : num  -0.978 -0.9889 -0.9643 -0.0522 0.2233 ...
##  $ tBodyAccJerk-std()-Y       : num  -0.9669 -0.9808 -0.9413 0.0751 -0.105 ...
##  $ tBodyAccJerk-std()-Z       : num  -0.976 -0.988 -0.976 -0.512 -0.318 ...
##  $ tBodyGyro-mean()-X         : num  -0.0196 -0.0432 -0.0282 0.0107 -0.1248 ...
##  $ tBodyGyro-mean()-Y         : num  -0.077 -0.068 -0.0877 -0.082 -0.1067 ...
##  $ tBodyGyro-mean()-Z         : num  0.1047 0.0746 0.1033 0.0987 0.0724 ...
##  $ tBodyGyro-std()-X          : num  -0.962 -0.989 -0.93 -0.414 -0.305 ...
##  $ tBodyGyro-std()-Y          : num  -0.954 -0.984 -0.959 -0.251 -0.311 ...
##  $ tBodyGyro-std()-Z          : num  -0.9719 -0.9604 -0.9537 -0.1745 -0.0353 ...
##  $ tBodyGyroJerk-mean()-X     : num  -0.1003 -0.0932 -0.1048 -0.1227 -0.0644 ...
##  $ tBodyGyroJerk-mean()-Y     : num  -0.0389 -0.0411 -0.0372 -0.0519 -0.0489 ...
##  $ tBodyGyroJerk-mean()-Z     : num  -0.0591 -0.0489 -0.0584 -0.0611 -0.072 ...
##  $ tBodyGyroJerk-std()-X      : num  -0.966 -0.992 -0.95 -0.366 -0.417 ...
##  $ tBodyGyroJerk-std()-Y      : num  -0.967 -0.993 -0.976 -0.51 -0.472 ...
##  $ tBodyGyroJerk-std()-Z      : num  -0.984 -0.99 -0.969 -0.329 -0.227 ...
##  $ tBodyAccMag-mean()         : num  -0.957 -0.961 -0.952 -0.127 0.251 ...
##  $ tBodyAccMag-std()          : num  -0.94 -0.94 -0.937 -0.186 0.157 ...
##  $ tGravityAccMag-mean()      : num  -0.957 -0.961 -0.952 -0.127 0.251 ...
##  $ tGravityAccMag-std()       : num  -0.94 -0.94 -0.937 -0.186 0.157 ...
##  $ tBodyAccJerkMag-mean()     : num  -0.9762 -0.9881 -0.9652 -0.1326 0.0507 ...
##  $ tBodyAccJerkMag-std()      : num  -0.9676 -0.9852 -0.9521 0.0376 0.1109 ...
##  $ tBodyGyroMag-mean()        : num  -0.9376 -0.9442 -0.9298 -0.1565 -0.0238 ...
##  $ tBodyGyroMag-std()         : num  -0.927 -0.963 -0.92 -0.402 -0.28 ...
##  $ tBodyGyroJerkMag-mean()    : num  -0.971 -0.994 -0.972 -0.44 -0.402 ...
##  $ tBodyGyroJerkMag-std()     : num  -0.96 -0.992 -0.964 -0.501 -0.494 ...
##  $ fBodyAcc-mean()-X          : num  -0.969 -0.985 -0.97 -0.114 0.216 ...
##  $ fBodyAcc-mean()-Y          : num  -0.95434 -0.94045 -0.92338 0.05339 0.00905 ...
##  $ fBodyAcc-mean()-Z          : num  -0.964 -0.975 -0.955 -0.412 -0.158 ...
##  $ fBodyAcc-std()-X           : num  -0.968 -0.982 -0.983 -0.206 0.325 ...
##  $ fBodyAcc-std()-Y           : num  -0.9462 -0.9128 -0.9219 -0.1298 -0.0633 ...
##  $ fBodyAcc-std()-Z           : num  -0.96 -0.966 -0.938 -0.435 -0.269 ...
##  $ fBodyAccJerk-mean()-X      : num  -0.979 -0.989 -0.964 -0.108 0.181 ...
##  $ fBodyAccJerk-mean()-Y      : num  -0.968 -0.981 -0.9422 0.0309 -0.1195 ...
##  $ fBodyAccJerk-mean()-Z      : num  -0.973 -0.986 -0.972 -0.464 -0.241 ...
##  $ fBodyAccJerk-std()-X       : num  -0.9789 -0.9896 -0.9686 -0.0792 0.1569 ...
##  $ fBodyAccJerk-std()-Y       : num  -0.9681 -0.982 -0.9448 0.0485 -0.1534 ...
##  $ fBodyAccJerk-std()-Z       : num  -0.979 -0.989 -0.979 -0.559 -0.396 ...
##  $ fBodyGyro-mean()-X         : num  -0.954 -0.986 -0.924 -0.278 -0.214 ...
##  $ fBodyGyro-mean()-Y         : num  -0.955 -0.986 -0.96 -0.345 -0.272 ...
##  $ fBodyGyro-mean()-Z         : num  -0.9698 -0.9654 -0.9509 -0.1085 0.0391 ...
##  $ fBodyGyro-std()-X          : num  -0.965 -0.99 -0.932 -0.458 -0.34 ...
##  $ fBodyGyro-std()-Y          : num  -0.953 -0.984 -0.958 -0.204 -0.343 ...
##  $ fBodyGyro-std()-Z          : num  -0.975 -0.962 -0.959 -0.273 -0.15 ...
##  $ fBodyAccMag-mean()         : num  -0.951 -0.955 -0.94 -0.093 0.22 ...
##  $ fBodyAccMag-std()          : num  -0.9443 -0.9415 -0.945 -0.3705 -0.0617 ...
##  $ fBodyBodyAccJerkMag-mean() : num  -0.9686 -0.9859 -0.9516 0.0205 0.157 ...
##  $ fBodyBodyAccJerkMag-std()  : num  -0.9654 -0.9833 -0.9521 0.0502 0.038 ...
##  $ fBodyBodyGyroMag-mean()    : num  -0.938 -0.974 -0.938 -0.413 -0.312 ...
##  $ fBodyBodyGyroMag-std()     : num  -0.934 -0.962 -0.923 -0.5 -0.388 ...
##  $ fBodyBodyGyroJerkMag-mean(): num  -0.961 -0.992 -0.966 -0.515 -0.484 ...
##  $ fBodyBodyGyroJerkMag-std() : num  -0.961 -0.991 -0.965 -0.518 -0.545 ...
```

```r
summary(tidy)
```

```
##    activity            subject    tBodyAcc-mean()-X tBodyAcc-mean()-Y 
##  Length:180         10     :  6   Min.   :0.222     Min.   :-0.04051  
##  Class :character   12     :  6   1st Qu.:0.271     1st Qu.:-0.02002  
##  Mode  :character   13     :  6   Median :0.277     Median :-0.01726  
##                     18     :  6   Mean   :0.274     Mean   :-0.01788  
##                     2      :  6   3rd Qu.:0.280     3rd Qu.:-0.01494  
##                     20     :  6   Max.   :0.301     Max.   :-0.00131  
##                     (Other):144                                       
##  tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y  tBodyAcc-std()-Z
##  Min.   :-0.1525   Min.   :-0.996   Min.   :-0.9902   Min.   :-0.988  
##  1st Qu.:-0.1121   1st Qu.:-0.980   1st Qu.:-0.9421   1st Qu.:-0.950  
##  Median :-0.1082   Median :-0.753   Median :-0.5090   Median :-0.652  
##  Mean   :-0.1092   Mean   :-0.558   Mean   :-0.4605   Mean   :-0.576  
##  3rd Qu.:-0.1044   3rd Qu.:-0.198   3rd Qu.:-0.0308   3rd Qu.:-0.231  
##  Max.   :-0.0754   Max.   : 0.627   Max.   : 0.6169   Max.   : 0.609  
##                                                                       
##  tGravityAcc-mean()-X tGravityAcc-mean()-Y tGravityAcc-mean()-Z
##  Min.   :-0.680       Min.   :-0.4799      Min.   :-0.4951     
##  1st Qu.: 0.838       1st Qu.:-0.2332      1st Qu.:-0.1173     
##  Median : 0.921       Median :-0.1278      Median : 0.0238     
##  Mean   : 0.698       Mean   :-0.0162      Mean   : 0.0741     
##  3rd Qu.: 0.942       3rd Qu.: 0.0877      3rd Qu.: 0.1495     
##  Max.   : 0.975       Max.   : 0.9566      Max.   : 0.9579     
##                                                                
##  tGravityAcc-std()-X tGravityAcc-std()-Y tGravityAcc-std()-Z
##  Min.   :-0.997      Min.   :-0.994      Min.   :-0.991     
##  1st Qu.:-0.983      1st Qu.:-0.971      1st Qu.:-0.961     
##  Median :-0.970      Median :-0.959      Median :-0.945     
##  Mean   :-0.964      Mean   :-0.952      Mean   :-0.936     
##  3rd Qu.:-0.951      3rd Qu.:-0.937      3rd Qu.:-0.918     
##  Max.   :-0.830      Max.   :-0.644      Max.   :-0.610     
##                                                             
##  tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z
##  Min.   :0.0427        Min.   :-0.03869      Min.   :-0.06746     
##  1st Qu.:0.0740        1st Qu.: 0.00047      1st Qu.:-0.01060     
##  Median :0.0764        Median : 0.00947      Median :-0.00386     
##  Mean   :0.0795        Mean   : 0.00757      Mean   :-0.00495     
##  3rd Qu.:0.0833        3rd Qu.: 0.01340      3rd Qu.: 0.00196     
##  Max.   :0.1302        Max.   : 0.05682      Max.   : 0.03805     
##                                                                   
##  tBodyAccJerk-std()-X tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z
##  Min.   :-0.995       Min.   :-0.990       Min.   :-0.993      
##  1st Qu.:-0.983       1st Qu.:-0.972       1st Qu.:-0.983      
##  Median :-0.810       Median :-0.776       Median :-0.884      
##  Mean   :-0.595       Mean   :-0.565       Mean   :-0.736      
##  3rd Qu.:-0.223       3rd Qu.:-0.148       3rd Qu.:-0.512      
##  Max.   : 0.544       Max.   : 0.355       Max.   : 0.031      
##                                                                
##  tBodyGyro-mean()-X tBodyGyro-mean()-Y tBodyGyro-mean()-Z
##  Min.   :-0.2058    Min.   :-0.2042    Min.   :-0.0724   
##  1st Qu.:-0.0471    1st Qu.:-0.0896    1st Qu.: 0.0747   
##  Median :-0.0287    Median :-0.0732    Median : 0.0851   
##  Mean   :-0.0324    Mean   :-0.0743    Mean   : 0.0874   
##  3rd Qu.:-0.0168    3rd Qu.:-0.0611    3rd Qu.: 0.1018   
##  Max.   : 0.1927    Max.   : 0.0275    Max.   : 0.1791   
##                                                          
##  tBodyGyro-std()-X tBodyGyro-std()-Y tBodyGyro-std()-Z
##  Min.   :-0.994    Min.   :-0.994    Min.   :-0.986   
##  1st Qu.:-0.974    1st Qu.:-0.963    1st Qu.:-0.961   
##  Median :-0.789    Median :-0.802    Median :-0.801   
##  Mean   :-0.692    Mean   :-0.653    Mean   :-0.616   
##  3rd Qu.:-0.441    3rd Qu.:-0.420    3rd Qu.:-0.311   
##  Max.   : 0.268    Max.   : 0.476    Max.   : 0.565   
##                                                       
##  tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
##  Min.   :-0.1572        Min.   :-0.0768        Min.   :-0.09250      
##  1st Qu.:-0.1032        1st Qu.:-0.0455        1st Qu.:-0.06172      
##  Median :-0.0987        Median :-0.0411        Median :-0.05343      
##  Mean   :-0.0961        Mean   :-0.0427        Mean   :-0.05480      
##  3rd Qu.:-0.0911        3rd Qu.:-0.0384        3rd Qu.:-0.04898      
##  Max.   :-0.0221        Max.   :-0.0132        Max.   :-0.00694      
##                                                                      
##  tBodyGyroJerk-std()-X tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z
##  Min.   :-0.997        Min.   :-0.997        Min.   :-0.995       
##  1st Qu.:-0.980        1st Qu.:-0.983        1st Qu.:-0.985       
##  Median :-0.840        Median :-0.894        Median :-0.861       
##  Mean   :-0.704        Mean   :-0.764        Mean   :-0.710       
##  3rd Qu.:-0.463        3rd Qu.:-0.586        3rd Qu.:-0.474       
##  Max.   : 0.179        Max.   : 0.296        Max.   : 0.193       
##                                                                   
##  tBodyAccMag-mean() tBodyAccMag-std() tGravityAccMag-mean()
##  Min.   :-0.9865    Min.   :-0.987    Min.   :-0.9865      
##  1st Qu.:-0.9573    1st Qu.:-0.943    1st Qu.:-0.9573      
##  Median :-0.4829    Median :-0.607    Median :-0.4829      
##  Mean   :-0.4973    Mean   :-0.544    Mean   :-0.4973      
##  3rd Qu.:-0.0919    3rd Qu.:-0.209    3rd Qu.:-0.0919      
##  Max.   : 0.6446    Max.   : 0.428    Max.   : 0.6446      
##                                                            
##  tGravityAccMag-std() tBodyAccJerkMag-mean() tBodyAccJerkMag-std()
##  Min.   :-0.987       Min.   :-0.993         Min.   :-0.995       
##  1st Qu.:-0.943       1st Qu.:-0.981         1st Qu.:-0.977       
##  Median :-0.607       Median :-0.817         Median :-0.801       
##  Mean   :-0.544       Mean   :-0.608         Mean   :-0.584       
##  3rd Qu.:-0.209       3rd Qu.:-0.246         3rd Qu.:-0.217       
##  Max.   : 0.428       Max.   : 0.434         Max.   : 0.451       
##                                                                   
##  tBodyGyroMag-mean() tBodyGyroMag-std() tBodyGyroJerkMag-mean()
##  Min.   :-0.981      Min.   :-0.981     Min.   :-0.9973        
##  1st Qu.:-0.946      1st Qu.:-0.948     1st Qu.:-0.9852        
##  Median :-0.655      Median :-0.742     Median :-0.8648        
##  Mean   :-0.565      Mean   :-0.630     Mean   :-0.7364        
##  3rd Qu.:-0.216      3rd Qu.:-0.360     3rd Qu.:-0.5119        
##  Max.   : 0.418      Max.   : 0.300     Max.   : 0.0876        
##                                                                
##  tBodyGyroJerkMag-std() fBodyAcc-mean()-X fBodyAcc-mean()-Y
##  Min.   :-0.998         Min.   :-0.995    Min.   :-0.9890  
##  1st Qu.:-0.981         1st Qu.:-0.979    1st Qu.:-0.9536  
##  Median :-0.881         Median :-0.769    Median :-0.5950  
##  Mean   :-0.755         Mean   :-0.576    Mean   :-0.4887  
##  3rd Qu.:-0.577         3rd Qu.:-0.217    3rd Qu.:-0.0634  
##  Max.   : 0.250         Max.   : 0.537    Max.   : 0.5242  
##                                                            
##  fBodyAcc-mean()-Z fBodyAcc-std()-X fBodyAcc-std()-Y  fBodyAcc-std()-Z
##  Min.   :-0.990    Min.   :-0.997   Min.   :-0.9907   Min.   :-0.987  
##  1st Qu.:-0.962    1st Qu.:-0.982   1st Qu.:-0.9404   1st Qu.:-0.946  
##  Median :-0.724    Median :-0.747   Median :-0.5134   Median :-0.644  
##  Mean   :-0.630    Mean   :-0.552   Mean   :-0.4815   Mean   :-0.582  
##  3rd Qu.:-0.318    3rd Qu.:-0.197   3rd Qu.:-0.0791   3rd Qu.:-0.266  
##  Max.   : 0.281    Max.   : 0.658   Max.   : 0.5602   Max.   : 0.687  
##                                                                       
##  fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
##  Min.   :-0.995        Min.   :-0.989        Min.   :-0.992       
##  1st Qu.:-0.983        1st Qu.:-0.973        1st Qu.:-0.980       
##  Median :-0.813        Median :-0.782        Median :-0.871       
##  Mean   :-0.614        Mean   :-0.588        Mean   :-0.714       
##  3rd Qu.:-0.282        3rd Qu.:-0.196        3rd Qu.:-0.470       
##  Max.   : 0.474        Max.   : 0.277        Max.   : 0.158       
##                                                                   
##  fBodyAccJerk-std()-X fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z
##  Min.   :-0.995       Min.   :-0.991       Min.   :-0.9931     
##  1st Qu.:-0.985       1st Qu.:-0.974       1st Qu.:-0.9837     
##  Median :-0.825       Median :-0.785       Median :-0.8951     
##  Mean   :-0.612       Mean   :-0.571       Mean   :-0.7565     
##  3rd Qu.:-0.247       3rd Qu.:-0.169       3rd Qu.:-0.5438     
##  Max.   : 0.477       Max.   : 0.350       Max.   :-0.0062     
##                                                                
##  fBodyGyro-mean()-X fBodyGyro-mean()-Y fBodyGyro-mean()-Z
##  Min.   :-0.993     Min.   :-0.994     Min.   :-0.986    
##  1st Qu.:-0.970     1st Qu.:-0.970     1st Qu.:-0.962    
##  Median :-0.730     Median :-0.814     Median :-0.791    
##  Mean   :-0.637     Mean   :-0.677     Mean   :-0.604    
##  3rd Qu.:-0.339     3rd Qu.:-0.446     3rd Qu.:-0.264    
##  Max.   : 0.475     Max.   : 0.329     Max.   : 0.492    
##                                                          
##  fBodyGyro-std()-X fBodyGyro-std()-Y fBodyGyro-std()-Z fBodyAccMag-mean()
##  Min.   :-0.995    Min.   :-0.994    Min.   :-0.987    Min.   :-0.987    
##  1st Qu.:-0.975    1st Qu.:-0.960    1st Qu.:-0.964    1st Qu.:-0.956    
##  Median :-0.809    Median :-0.796    Median :-0.822    Median :-0.670    
##  Mean   :-0.711    Mean   :-0.645    Mean   :-0.658    Mean   :-0.536    
##  3rd Qu.:-0.481    3rd Qu.:-0.415    3rd Qu.:-0.392    3rd Qu.:-0.162    
##  Max.   : 0.197    Max.   : 0.646    Max.   : 0.522    Max.   : 0.587    
##                                                                          
##  fBodyAccMag-std() fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
##  Min.   :-0.988    Min.   :-0.994             Min.   :-0.994           
##  1st Qu.:-0.945    1st Qu.:-0.977             1st Qu.:-0.975           
##  Median :-0.651    Median :-0.794             Median :-0.813           
##  Mean   :-0.621    Mean   :-0.576             Mean   :-0.599           
##  3rd Qu.:-0.365    3rd Qu.:-0.187             3rd Qu.:-0.267           
##  Max.   : 0.179    Max.   : 0.538             Max.   : 0.316           
##                                                                        
##  fBodyBodyGyroMag-mean() fBodyBodyGyroMag-std()
##  Min.   :-0.987          Min.   :-0.982        
##  1st Qu.:-0.962          1st Qu.:-0.949        
##  Median :-0.766          Median :-0.773        
##  Mean   :-0.667          Mean   :-0.672        
##  3rd Qu.:-0.409          3rd Qu.:-0.428        
##  Max.   : 0.204          Max.   : 0.237        
##                                                
##  fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
##  Min.   :-0.998              Min.   :-0.998            
##  1st Qu.:-0.981              1st Qu.:-0.980            
##  Median :-0.878              Median :-0.894            
##  Mean   :-0.756              Mean   :-0.771            
##  3rd Qu.:-0.583              3rd Qu.:-0.608            
##  Max.   : 0.147              Max.   : 0.288            
## 
```

```r
head(tidy)
```

```
##             activity subject tBodyAcc-mean()-X tBodyAcc-mean()-Y
## 1             LAYING      10            0.2802          -0.02429
## 2            SITTING      10            0.2706          -0.01504
## 3           STANDING      10            0.2767          -0.01554
## 4            WALKING      10            0.2786          -0.01702
## 5 WALKING_DOWNSTAIRS      10            0.2904          -0.02001
## 6   WALKING_UPSTAIRS      10            0.2671          -0.01439
##   tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
## 1           -0.1172          -0.9683        -0.946454         -0.95947
## 2           -0.1043          -0.9829        -0.917980         -0.96783
## 3           -0.1080          -0.9784        -0.919562         -0.94127
## 4           -0.1091          -0.1787        -0.022743         -0.39565
## 5           -0.1108           0.2957         0.004079         -0.18356
## 6           -0.1182          -0.1616        -0.005553         -0.07387
##   tGravityAcc-mean()-X tGravityAcc-mean()-Y tGravityAcc-mean()-Z
## 1              -0.4531             -0.13930             -0.03112
## 2               0.7919             -0.04126              0.20253
## 3               0.9540             -0.04316              0.03470
## 4               0.9631             -0.08383              0.05493
## 5               0.9398             -0.06462              0.04187
## 6               0.9319             -0.05657              0.02275
##   tGravityAcc-std()-X tGravityAcc-std()-Y tGravityAcc-std()-Z
## 1             -0.9545             -0.9667             -0.9630
## 2             -0.9731             -0.9357             -0.9669
## 3             -0.9913             -0.9754             -0.9596
## 4             -0.9744             -0.9711             -0.9579
## 5             -0.9393             -0.9370             -0.9021
## 6             -0.9592             -0.9370             -0.8647
##   tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z
## 1               0.07382              0.015699             0.0071669
## 2               0.07754              0.008979            -0.0049957
## 3               0.08098              0.011901            -0.0048584
## 4               0.08579              0.004082            -0.0162953
## 5               0.10096              0.010782             0.0001526
## 6               0.06488              0.026631            -0.0513376
##   tBodyAccJerk-std()-X tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z
## 1              -0.9780             -0.96693              -0.9763
## 2              -0.9889             -0.98076              -0.9883
## 3              -0.9643             -0.94133              -0.9764
## 4              -0.0522              0.07508              -0.5117
## 5               0.2233             -0.10502              -0.3182
## 6              -0.1872             -0.17780              -0.6075
##   tBodyGyro-mean()-X tBodyGyro-mean()-Y tBodyGyro-mean()-Z
## 1           -0.01956           -0.07703            0.10472
## 2           -0.04324           -0.06800            0.07460
## 3           -0.02819           -0.08768            0.10325
## 4            0.01069           -0.08195            0.09867
## 5           -0.12484           -0.10674            0.07244
## 6            0.07334           -0.09546            0.08797
##   tBodyGyro-std()-X tBodyGyro-std()-Y tBodyGyro-std()-Z
## 1           -0.9617          -0.95366           -0.9719
## 2           -0.9889          -0.98437           -0.9604
## 3           -0.9299          -0.95886           -0.9537
## 4           -0.4142          -0.25085           -0.1745
## 5           -0.3047          -0.31141           -0.0353
## 6           -0.3085           0.04117           -0.3205
##   tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
## 1               -0.10031               -0.03888               -0.05907
## 2               -0.09316               -0.04112               -0.04894
## 3               -0.10475               -0.03716               -0.05842
## 4               -0.12271               -0.05192               -0.06113
## 5               -0.06438               -0.04894               -0.07204
## 6               -0.15721               -0.03723               -0.03650
##   tBodyGyroJerk-std()-X tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z
## 1               -0.9659               -0.9666               -0.9839
## 2               -0.9923               -0.9926               -0.9903
## 3               -0.9504               -0.9764               -0.9691
## 4               -0.3661               -0.5097               -0.3291
## 5               -0.4170               -0.4717               -0.2271
## 6               -0.4273               -0.6046               -0.4822
##   tBodyAccMag-mean() tBodyAccMag-std() tGravityAccMag-mean()
## 1           -0.95678           -0.9403              -0.95678
## 2           -0.96076           -0.9397              -0.96076
## 3           -0.95199           -0.9373              -0.95199
## 4           -0.12740           -0.1856              -0.12740
## 5            0.25084            0.1570               0.25084
## 6           -0.02666           -0.2115              -0.02666
##   tGravityAccMag-std() tBodyAccJerkMag-mean() tBodyAccJerkMag-std()
## 1              -0.9403                -0.9762              -0.96756
## 2              -0.9397                -0.9881              -0.98521
## 3              -0.9373                -0.9652              -0.95206
## 4              -0.1856                -0.1326               0.03761
## 5               0.1570                 0.0507               0.11088
## 6              -0.2115                -0.2620              -0.22601
##   tBodyGyroMag-mean() tBodyGyroMag-std() tBodyGyroJerkMag-mean()
## 1            -0.93759            -0.9275                 -0.9708
## 2            -0.94420            -0.9631                 -0.9937
## 3            -0.92979            -0.9205                 -0.9716
## 4            -0.15646            -0.4020                 -0.4404
## 5            -0.02385            -0.2804                 -0.4019
## 6             0.04386            -0.1131                 -0.5235
##   tBodyGyroJerkMag-std() fBodyAcc-mean()-X fBodyAcc-mean()-Y
## 1                -0.9596           -0.9692         -0.954342
## 2                -0.9917           -0.9849         -0.940450
## 3                -0.9644           -0.9701         -0.923379
## 4                -0.5010           -0.1142          0.053391
## 5                -0.4941            0.2157          0.009047
## 6                -0.6183           -0.1394         -0.039069
##   fBodyAcc-mean()-Z fBodyAcc-std()-X fBodyAcc-std()-Y fBodyAcc-std()-Z
## 1           -0.9643          -0.9680         -0.94623         -0.95981
## 2           -0.9746          -0.9822         -0.91284         -0.96612
## 3           -0.9553          -0.9829         -0.92186         -0.93810
## 4           -0.4122          -0.2064         -0.12978         -0.43530
## 5           -0.1577           0.3250         -0.06333         -0.26869
## 6           -0.3012          -0.1720         -0.05163         -0.03423
##   fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
## 1               -0.9790              -0.96804               -0.9726
## 2               -0.9894              -0.98096               -0.9861
## 3               -0.9636              -0.94219               -0.9722
## 4               -0.1079               0.03091               -0.4640
## 5                0.1814              -0.11950               -0.2407
## 6               -0.2012              -0.20687               -0.5532
##   fBodyAccJerk-std()-X fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z
## 1             -0.97890             -0.96812              -0.9786
## 2             -0.98955             -0.98205              -0.9891
## 3             -0.96861             -0.94485              -0.9794
## 4             -0.07915              0.04849              -0.5585
## 5              0.15691             -0.15336              -0.3961
## 6             -0.24760             -0.20280              -0.6629
##   fBodyGyro-mean()-X fBodyGyro-mean()-Y fBodyGyro-mean()-Z
## 1            -0.9538            -0.9547           -0.96976
## 2            -0.9865            -0.9862           -0.96540
## 3            -0.9240            -0.9603           -0.95092
## 4            -0.2779            -0.3452           -0.10853
## 5            -0.2144            -0.2725            0.03914
## 6            -0.2185            -0.2359           -0.26276
##   fBodyGyro-std()-X fBodyGyro-std()-Y fBodyGyro-std()-Z fBodyAccMag-mean()
## 1           -0.9645           -0.9535           -0.9753           -0.95085
## 2           -0.9897           -0.9836           -0.9624           -0.95528
## 3           -0.9323           -0.9584           -0.9589           -0.94011
## 4           -0.4583           -0.2044           -0.2733           -0.09302
## 5           -0.3395           -0.3432           -0.1504            0.22012
## 6           -0.3413            0.1717           -0.4038           -0.16687
##   fBodyAccMag-std() fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
## 1          -0.94430                   -0.96856                  -0.96538
## 2          -0.94151                   -0.98590                  -0.98327
## 3          -0.94505                   -0.95163                  -0.95211
## 4          -0.37047                    0.02052                   0.05021
## 5          -0.06167                    0.15698                   0.03803
## 6          -0.36144                   -0.15355                  -0.33437
##   fBodyBodyGyroMag-mean() fBodyBodyGyroMag-std()
## 1                 -0.9377                -0.9337
## 2                 -0.9745                -0.9625
## 3                 -0.9378                -0.9230
## 4                 -0.4126                -0.4998
## 5                 -0.3123                -0.3878
## 6                 -0.2654                -0.1670
##   fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
## 1                     -0.9610                    -0.9608
## 2                     -0.9922                    -0.9914
## 3                     -0.9656                    -0.9651
## 4                     -0.5151                    -0.5177
## 5                     -0.4842                    -0.5450
## 6                     -0.6006                    -0.6713
```

... and write it to disk.


```r
write.table(tidy, file="tidy.txt")
```

You can also embed plots, for example:


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
dat$activity <- as.factor(dat$activity)
BodyAcc <- dat[,3]
Subject <- dat$subject
Activity <- dat$activity
qplot(Subject, BodyAcc, col=Activity)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


