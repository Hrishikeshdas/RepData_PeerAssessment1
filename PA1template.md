author: "Hrishikesh Das"
date: "July 4, 2018"

```r
activity <- read.csv("activity.csv")
com_activity <- activity[complete.cases(activity),]
```

## What is mean total number of steps taken per day?


```r
meantotalnumber <- aggregate(steps ~ date, com_activity, FUN = mean, na.rm = TRUE)
totalsteps <- aggregate(steps ~ date, com_activity, FUN = sum, na.rm = TRUE)
hist(totalsteps$steps, main = "Histogram of the total number of steps taken each day", xlab = "Steps taken per day")
```



## Calculate and report the mean and median


```r
round(mean(totalsteps$steps))
```

```
## [1] 10766
```

```r
median(totalsteps$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

```r
averagepattern_interval <- aggregate(steps ~ interval, com_activity, FUN = mean, na.rm = TRUE)
plot(averagepattern_interval$interval, averagepattern_interval$steps, type = "l", col = 1,  main = "Average daily activity pattern", xlab = "Time Interval", ylab = "Average no of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
## of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_index <- which.max(averagepattern_interval$steps)
print (paste("The interval with the highest avg steps is ", averagepattern_interval[interval_index, ]$interval, " and the no of steps for that interval is ", 
             round(averagepattern_interval[interval_index, ]$steps, digits = 1)))
```

```
## [1] "The interval with the highest avg steps is  835  and the no of steps for that interval is  206.2"
```

## Imputing missing values

```r
totalmissing <- activity[!complete.cases(activity),]
nrow(totalmissing)
```

```
## [1] 2304
```

```r
for(i in 1:nrow(activity)) {
  if(is.na(activity$steps[i])) {
    val <- averagepattern_interval$steps[which(averagepattern_interval$interval == activity$interval[i])]
    activity$steps[i] <- val
  }
}
```

```r
newdata <- aggregate(steps ~ date, activity, sum)
```
## Make a histogram of the total number of steps taken each day

```r
hist(newdata$steps, main = "Histogram of the total number of steps taken each day in new set of data", xlab = "Steps taken per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
round(mean(newdata$steps))
```

```
## [1] 10766
```

```r
median(newdata$steps)
```

```
## [1] 10766.19
```

```r
week_day <- function(date_val) {
  wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}
```
## Make a panel plot containing a time series plot

```r
activity$day_type <- as.factor(sapply(activity$date, week_day))
steps_perday <- aggregate(steps ~ interval + day_type, activity, mean)
library(ggplot2)
ggplot(steps_perday, aes(interval, steps, col = day_type)) +
  geom_point() +
  geom_line() +
  facet_grid(day_type~., scales="fixed", space="fixed") +
  ggtitle("No. of Steps interval per day") +
  xlab("Steps") +
  ylab("Interval") +
  scale_colour_discrete(name = "Day Type")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

