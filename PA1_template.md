

# Activity monitoring exploratory data analysis

### Dataset Information

The data used here is available from: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### 1. Loading and Preprocessing the data

Let's load and preprocess the data!


```r
zipF <- "repdata_data_activity.zip"
outDir <- "activity"
unzip(zipfile = zipF, exdir = outDir)

activity <- read.csv("activity/activity.csv", header = TRUE, sep = ",")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
dates <- unique(activity$date)
intervals <- unique(activity$interval)
```

### 2. Mean and Median total number of steps taken per day

Calculating number of steps taken each day


```r
stepsSplit <- split(activity$steps, activity$date)
totalStepsPerDay <- sapply(stepsSplit, sum, na.rm=TRUE)
```

Histogram of the total number of steps taken each day


```r
plot(dates, totalStepsPerDay, main="Histogram of steps taken each day", xlab="Date", ylab="Frequency", type="h", lwd = 10)
```

![plot of chunk histogram](figure/histogram-1.png)

Mean


```r
meanStepsPerDay <- sapply(stepsSplit, mean, na.rm=TRUE)
meanDf <- data.frame(dates, meanStepsPerDay, row.names=NULL)
meanDf
```

```
##         dates meanStepsPerDay
## 1  2012-10-01             NaN
## 2  2012-10-02       0.4375000
## 3  2012-10-03      39.4166667
## 4  2012-10-04      42.0694444
## 5  2012-10-05      46.1597222
## 6  2012-10-06      53.5416667
## 7  2012-10-07      38.2465278
## 8  2012-10-08             NaN
## 9  2012-10-09      44.4826389
## 10 2012-10-10      34.3750000
## 11 2012-10-11      35.7777778
## 12 2012-10-12      60.3541667
## 13 2012-10-13      43.1458333
## 14 2012-10-14      52.4236111
## 15 2012-10-15      35.2048611
## 16 2012-10-16      52.3750000
## 17 2012-10-17      46.7083333
## 18 2012-10-18      34.9166667
## 19 2012-10-19      41.0729167
## 20 2012-10-20      36.0937500
## 21 2012-10-21      30.6284722
## 22 2012-10-22      46.7361111
## 23 2012-10-23      30.9652778
## 24 2012-10-24      29.0104167
## 25 2012-10-25       8.6527778
## 26 2012-10-26      23.5347222
## 27 2012-10-27      35.1354167
## 28 2012-10-28      39.7847222
## 29 2012-10-29      17.4236111
## 30 2012-10-30      34.0937500
## 31 2012-10-31      53.5208333
## 32 2012-11-01             NaN
## 33 2012-11-02      36.8055556
## 34 2012-11-03      36.7048611
## 35 2012-11-04             NaN
## 36 2012-11-05      36.2465278
## 37 2012-11-06      28.9375000
## 38 2012-11-07      44.7326389
## 39 2012-11-08      11.1770833
## 40 2012-11-09             NaN
## 41 2012-11-10             NaN
## 42 2012-11-11      43.7777778
## 43 2012-11-12      37.3784722
## 44 2012-11-13      25.4722222
## 45 2012-11-14             NaN
## 46 2012-11-15       0.1423611
## 47 2012-11-16      18.8923611
## 48 2012-11-17      49.7881944
## 49 2012-11-18      52.4652778
## 50 2012-11-19      30.6979167
## 51 2012-11-20      15.5277778
## 52 2012-11-21      44.3993056
## 53 2012-11-22      70.9270833
## 54 2012-11-23      73.5902778
## 55 2012-11-24      50.2708333
## 56 2012-11-25      41.0902778
## 57 2012-11-26      38.7569444
## 58 2012-11-27      47.3819444
## 59 2012-11-28      35.3576389
## 60 2012-11-29      24.4687500
## 61 2012-11-30             NaN
```

Median


```r
medianStepsPerDay <- sapply(stepsSplit, median, na.rm=TRUE)
medianDf <- data.frame(dates, medianStepsPerDay, row.names=NULL)
medianDf
```

```
##         dates medianStepsPerDay
## 1  2012-10-01                NA
## 2  2012-10-02                 0
## 3  2012-10-03                 0
## 4  2012-10-04                 0
## 5  2012-10-05                 0
## 6  2012-10-06                 0
## 7  2012-10-07                 0
## 8  2012-10-08                NA
## 9  2012-10-09                 0
## 10 2012-10-10                 0
## 11 2012-10-11                 0
## 12 2012-10-12                 0
## 13 2012-10-13                 0
## 14 2012-10-14                 0
## 15 2012-10-15                 0
## 16 2012-10-16                 0
## 17 2012-10-17                 0
## 18 2012-10-18                 0
## 19 2012-10-19                 0
## 20 2012-10-20                 0
## 21 2012-10-21                 0
## 22 2012-10-22                 0
## 23 2012-10-23                 0
## 24 2012-10-24                 0
## 25 2012-10-25                 0
## 26 2012-10-26                 0
## 27 2012-10-27                 0
## 28 2012-10-28                 0
## 29 2012-10-29                 0
## 30 2012-10-30                 0
## 31 2012-10-31                 0
## 32 2012-11-01                NA
## 33 2012-11-02                 0
## 34 2012-11-03                 0
## 35 2012-11-04                NA
## 36 2012-11-05                 0
## 37 2012-11-06                 0
## 38 2012-11-07                 0
## 39 2012-11-08                 0
## 40 2012-11-09                NA
## 41 2012-11-10                NA
## 42 2012-11-11                 0
## 43 2012-11-12                 0
## 44 2012-11-13                 0
## 45 2012-11-14                NA
## 46 2012-11-15                 0
## 47 2012-11-16                 0
## 48 2012-11-17                 0
## 49 2012-11-18                 0
## 50 2012-11-19                 0
## 51 2012-11-20                 0
## 52 2012-11-21                 0
## 53 2012-11-22                 0
## 54 2012-11-23                 0
## 55 2012-11-24                 0
## 56 2012-11-25                 0
## 57 2012-11-26                 0
## 58 2012-11-27                 0
## 59 2012-11-28                 0
## 60 2012-11-29                 0
## 61 2012-11-30                NA
```

### 3. Average daily activity pattern

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
intervalSplit <- split(activity$steps, activity$interval)
averageStepsPerInterval <- sapply(intervalSplit, mean, na.rm=TRUE)
plot(intervals, averageStepsPerInterval, type="l",
     main="Average number of steps per interval across all days", 
     xlab="Interval", ylab="Frequency")
```

![plot of chunk timeseries](figure/timeseries-1.png)

Interval with maximum number of steps


```r
maxIntervalDays <- max(averageStepsPerInterval, na.rm=TRUE)
maxIndex <- as.numeric(which(averageStepsPerInterval == maxIntervalDays))
paste("Interval with maximum number of steps: ", intervals[maxIndex])
```

```
## [1] "Interval with maximum number of steps:  835"
```

### 4. Imputing missing values

Calculating the total number of missing values in the dataset 


```r
noNAs <- sum(is.na(activity$steps))
paste("Total number of missing values: ", noNAs)
```

```
## [1] "Total number of missing values:  2304"
```

Replacing NA with mean total number of steps per day


```r
meanSteps <- mean(meanStepsPerDay, na.rm = TRUE)
newActivity <- activity
newActivity$steps[which(is.na(activity$steps))] <- meanSteps
```

Histogram of the total number of steps taken each day with new datset


```r
nstepsSplit <- split(newActivity$steps, newActivity$date)
ntotalStepsPerDay <- sapply(nstepsSplit, sum, na.rm=TRUE)
plot(dates, ntotalStepsPerDay, main="Histogram of steps taken each day", xlab="Date", ylab="Frequency", type="h", lwd = 10)
```

![plot of chunk newhist](figure/newhist-1.png)

Mean and median total number of steps taken per day

New Mean


```r
nmeanStepsPerDay <- sapply(nstepsSplit, mean, na.rm=TRUE)
nmeanDf <- data.frame(dates, nmeanStepsPerDay, row.names=NULL)
nmeanDf
```

```
##         dates nmeanStepsPerDay
## 1  2012-10-01       37.3825996
## 2  2012-10-02        0.4375000
## 3  2012-10-03       39.4166667
## 4  2012-10-04       42.0694444
## 5  2012-10-05       46.1597222
## 6  2012-10-06       53.5416667
## 7  2012-10-07       38.2465278
## 8  2012-10-08       37.3825996
## 9  2012-10-09       44.4826389
## 10 2012-10-10       34.3750000
## 11 2012-10-11       35.7777778
## 12 2012-10-12       60.3541667
## 13 2012-10-13       43.1458333
## 14 2012-10-14       52.4236111
## 15 2012-10-15       35.2048611
## 16 2012-10-16       52.3750000
## 17 2012-10-17       46.7083333
## 18 2012-10-18       34.9166667
## 19 2012-10-19       41.0729167
## 20 2012-10-20       36.0937500
## 21 2012-10-21       30.6284722
## 22 2012-10-22       46.7361111
## 23 2012-10-23       30.9652778
## 24 2012-10-24       29.0104167
## 25 2012-10-25        8.6527778
## 26 2012-10-26       23.5347222
## 27 2012-10-27       35.1354167
## 28 2012-10-28       39.7847222
## 29 2012-10-29       17.4236111
## 30 2012-10-30       34.0937500
## 31 2012-10-31       53.5208333
## 32 2012-11-01       37.3825996
## 33 2012-11-02       36.8055556
## 34 2012-11-03       36.7048611
## 35 2012-11-04       37.3825996
## 36 2012-11-05       36.2465278
## 37 2012-11-06       28.9375000
## 38 2012-11-07       44.7326389
## 39 2012-11-08       11.1770833
## 40 2012-11-09       37.3825996
## 41 2012-11-10       37.3825996
## 42 2012-11-11       43.7777778
## 43 2012-11-12       37.3784722
## 44 2012-11-13       25.4722222
## 45 2012-11-14       37.3825996
## 46 2012-11-15        0.1423611
## 47 2012-11-16       18.8923611
## 48 2012-11-17       49.7881944
## 49 2012-11-18       52.4652778
## 50 2012-11-19       30.6979167
## 51 2012-11-20       15.5277778
## 52 2012-11-21       44.3993056
## 53 2012-11-22       70.9270833
## 54 2012-11-23       73.5902778
## 55 2012-11-24       50.2708333
## 56 2012-11-25       41.0902778
## 57 2012-11-26       38.7569444
## 58 2012-11-27       47.3819444
## 59 2012-11-28       35.3576389
## 60 2012-11-29       24.4687500
## 61 2012-11-30       37.3825996
```

New Median


```r
nmedianStepsPerDay <- sapply(nstepsSplit, median, na.rm=TRUE)
nmedianDf <- data.frame(dates, nmedianStepsPerDay, row.names=NULL)
nmedianDf
```

```
##         dates nmedianStepsPerDay
## 1  2012-10-01            37.3826
## 2  2012-10-02             0.0000
## 3  2012-10-03             0.0000
## 4  2012-10-04             0.0000
## 5  2012-10-05             0.0000
## 6  2012-10-06             0.0000
## 7  2012-10-07             0.0000
## 8  2012-10-08            37.3826
## 9  2012-10-09             0.0000
## 10 2012-10-10             0.0000
## 11 2012-10-11             0.0000
## 12 2012-10-12             0.0000
## 13 2012-10-13             0.0000
## 14 2012-10-14             0.0000
## 15 2012-10-15             0.0000
## 16 2012-10-16             0.0000
## 17 2012-10-17             0.0000
## 18 2012-10-18             0.0000
## 19 2012-10-19             0.0000
## 20 2012-10-20             0.0000
## 21 2012-10-21             0.0000
## 22 2012-10-22             0.0000
## 23 2012-10-23             0.0000
## 24 2012-10-24             0.0000
## 25 2012-10-25             0.0000
## 26 2012-10-26             0.0000
## 27 2012-10-27             0.0000
## 28 2012-10-28             0.0000
## 29 2012-10-29             0.0000
## 30 2012-10-30             0.0000
## 31 2012-10-31             0.0000
## 32 2012-11-01            37.3826
## 33 2012-11-02             0.0000
## 34 2012-11-03             0.0000
## 35 2012-11-04            37.3826
## 36 2012-11-05             0.0000
## 37 2012-11-06             0.0000
## 38 2012-11-07             0.0000
## 39 2012-11-08             0.0000
## 40 2012-11-09            37.3826
## 41 2012-11-10            37.3826
## 42 2012-11-11             0.0000
## 43 2012-11-12             0.0000
## 44 2012-11-13             0.0000
## 45 2012-11-14            37.3826
## 46 2012-11-15             0.0000
## 47 2012-11-16             0.0000
## 48 2012-11-17             0.0000
## 49 2012-11-18             0.0000
## 50 2012-11-19             0.0000
## 51 2012-11-20             0.0000
## 52 2012-11-21             0.0000
## 53 2012-11-22             0.0000
## 54 2012-11-23             0.0000
## 55 2012-11-24             0.0000
## 56 2012-11-25             0.0000
## 57 2012-11-26             0.0000
## 58 2012-11-27             0.0000
## 59 2012-11-28             0.0000
## 60 2012-11-29             0.0000
## 61 2012-11-30            37.3826
```

We can see that the only values that have changed are those days where all of the observations were missing. The rest of the observations are the same.

### 5.Differences in activity patterns between weekdays and weekends

Seperating days into weekends and weekdays


```r
newActivity$weekday <- weekdays(newActivity$date)
newActivity$DayType <- ifelse(newActivity$weekday=='Saturday' | newActivity$weekday=='Sunday', 'weekend','weekday')
```

Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 


```r
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=newActivity,FUN=mean)
library(ggplot2)
j <- ggplot(StepsPerTimeDT, aes(interval, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
