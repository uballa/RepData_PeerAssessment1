# Reporducible Research: Peer Assessment 1
========================================================
### CreatedBy - Uday Kiran
### CreatedOn - 17 July 2014

**Note**:  Running code requires **"activity.csv"** to be in working directory 

## Loading and preprocessing the data

```r
dt <- read.csv("activity.csv")
dt$DateTime <- strptime(paste(dt$date, paste(as.character(dt$interval%/%100), as.character(dt$interval%%100), sep=":"), sep=" "), "%Y-%m-%d %H:%M")
```


## What is mean total number of steps taken per day?

```r
aggdata <- aggregate(dt$steps, by=list(Category=dt$date), FUN=sum)
colnames(aggdata)[2] <- "steps"
```
Histogram of the total number of steps taken each day

```r
hist(aggdata$steps, main = paste("Histogram of" , "the total number of steps taken each day"), xlab = "total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
Mean of the total number of steps taken each day

```r
mean(aggdata$steps, na.rm=TRUE)
```

```
## [1] 10766
```
Median of the total number of steps taken each day

```r
median(aggdata$steps, na.rm=TRUE)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

```r
dt.interval <- aggregate(dt$steps, by=list(Category=dt$interval), FUN=mean, na.rm=TRUE)
```
Time series plot

```r
plot(dt.interval$Category, dt.interval$x, ylab="Average number of steps taken", xlab="5-minute interval", type="n")
lines(dt.interval$Category, dt.interval$x, type= "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- max(dt$steps, na.rm=TRUE)
dtWithoutNA <- na.omit(dt)
dtWithoutNA[dtWithoutNA$steps == maxSteps, ]
```

```
##       steps       date interval            DateTime
## 16492   806 2012-11-27      615 2012-11-27 06:15:00
```

## Imputing missing values

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!is.na(dt$steps))
```

```
## [1] 15264
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Use mean for the 5 minute interval for the missing values.

```r
aggdatabyInterval <- aggregate(dt$steps, by=list(Category=dt$interval), FUN=mean, na.rm=TRUE)
colnames(aggdatabyInterval)[2] <- "meansteps"
colnames(aggdatabyInterval)[1] <- "interval"
```
3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dt <- merge(dt, aggdatabyInterval)
dt$steps[is.na(dt$steps)] = dt$meansteps
```

```
## Warning: number of items to replace is not a multiple of replacement
## length
```
4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
aggdata <- aggregate(dt$steps, by=list(Category=dt$date), FUN=sum)
colnames(aggdata)[2] <- "steps"
```
Histogram of the total number of steps taken each day

```r
hist(aggdata$steps, main = paste("Histogram of" , "the total number of steps taken each day"), xlab = "total number of steps taken each day")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
Mean of the total number of steps taken each day

```r
mean(aggdata$steps, na.rm=TRUE)
```

```
## [1] 9371
```
Median of the total number of steps taken each day

```r
median(aggdata$steps, na.rm=TRUE)
```

```
## [1] 10395
```
So the mean and median are different now.
## Are there differences in activity patterns between weekdays and weekends?

```r
dt$day <- weekdays(as.Date(dt$date))
weekenddt <- subset(dt, dt$day == "Saturday" | dt$day == "Sunday")
weekdaydt <- subset(dt, !(dt$day == "Saturday" | dt$day == "Sunday"))
aggdataweekend <- aggregate(weekenddt$steps, by=list(Category=weekenddt$interval), FUN=mean, na.rm=TRUE)
colnames(aggdataweekend)[2] <- "weekend"
aggdataweekday <- aggregate(weekdaydt$steps, by=list(Category=weekdaydt$interval), FUN=mean, na.rm=TRUE)
colnames(aggdataweekday)[2] <- "weekday"
aggdatamerge <- merge(aggdataweekday, aggdataweekend)
colnames(aggdatamerge)[1] <- "Interval"
library(reshape2)
mm <- melt(aggdatamerge,id.var="Interval")
library(lattice)
```


```r
xyplot(value~Interval|variable,data=mm,type="l", ylab = "Number of steps",
       scales=list(y=list(relation="free")),
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 
