Reproducible Research: Peer Assessment 1
========================================================
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
The variables included in this dataset are:  
- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- **date**: The date on which the measurement was taken in YYYY-MM-DD format
- **intervals**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

First download the csv file and upload it to R. 

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

Below is a histogram of the total number of steps taken each day.


```r
# the total number of steps taken each day
totalSteps <- tapply(data$steps, data$date, sum, na.rm = T)
```



```r
# make a historgram of the total number of steps taken each day
hist(totalSteps, breaks = 10)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


the mean and median total number of steps taken per day are 9354.23 and 10395, respectively.

```r
meanSteps <- mean(totalSteps)
medianSteps <- median(totalSteps)
```


## What is the average daily activity pattern?
below is a a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
# Make a time series plot (i.e. type = 'l') of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all days
# (y-axis)

avSteps <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(data$interval[1:288], avSteps, type = "l", xlab = "interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


Interval 835 contains the maximum number of steps on average across all the days in the dataset.

```r
# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
names(which(avSteps == max(avSteps)))
```

```
## [1] "835"
```


## Imputing missing values

the total number of missing values n the dataset is 2304.

```r
# the total number of missing values in the dataset
sum(is.na(data$steps))
```

```
## [1] 2304
```


TO fill in all of the missing values in the dataset, I use the mean for that day.

```r
# Devise a strategy for filling in all of the missing values in the dataset.
# use the mean for that day
meanD <- totalSteps/288
mT <- rep(meanD, each = 288)

# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
repData <- data
for (i in 1:17568) {
    if (is.na(repData$steps[i])) 
        repData$steps[i] <- mT[i]
}

# Make a histogram of the total number of steps taken each day
totalStepsM <- tapply(repData$steps, repData$date, sum)
hist(totalStepsM, breaks = 10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r

# Calculate and report the mean and median total number of steps taken per
# day
meanStepsM <- mean(totalStepsM)
medianStepsM <- median(totalStepsM)
```

The mean and the median total number of steps taken per day are exactly the same as prior to imputing missing value. Thus te two histograms are the same. This is because all the missing values come from special days where all the steps are missing. I am replacing missing values with the mean for the day, that is, 0. As a result, the total number of steps taken each day doesn't change with the imputing missing data.

## Are there differences in activity patterns between weekdays and weekends?

Below are plots showing the differences in activity patterns between weekdays and weekends.

```r
# transform to date
repData$date <- as.Date(repData$date, format = "%Y-%m-%d")

# create a factor variable with two levels -
#'weekday' and 'weekend'
weekday <- ifelse(weekdays(repData$date) == "Sunday", "Weekend", "Weekday")
repData$weekday <- as.factor(weekday)

# Make a panel plot containing a time series plot (i.e. type = 'l') of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis).
avgStepsW <- tapply(data$steps, list(repData$interval, repData$weekday), mean, 
    na.rm = T)
news1 <- data.frame(interval = data$interval[1:288], Weekday = rep("Weekday", 
    each = 288), avgStep = avgStepsW[, 1])
news2 <- data.frame(interval = data$interval[1:288], Weekday = rep("Weekend", 
    each = 288), avgStep = avgStepsW[, 2])
news <- rbind(news1, news2)
library(ggplot2)
qplot(interval, avgStep, data = news, facets = Weekday ~ ., geom = "line", xlab = "interval", 
    ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

