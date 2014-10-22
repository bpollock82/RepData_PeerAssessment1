# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity<-read.csv("activity.csv",colClasses = c("numeric","character","numeric"),sep=",")
activity$date<-as.Date(activity$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
totalsteps<-aggregate(activity$steps~activity$date,data=activity,sum,na.rm=TRUE)
hist(totalsteps$`activity$steps`, main="Total Steps Per Day",xlab="Days",col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(totalsteps$`activity$steps`)
```

```
## [1] 10766.19
```

```r
median(totalsteps$`activity$steps`)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
time_series<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
plot(row.names(time_series),time_series,type="l",main="Avergae Number of Steps Taken",xlab="5 Minute Interval",ylab="Average Across All Days",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max<-which.max(time_series)
names(max)
```

```
## [1] "835"
```

## Imputing missing values


```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
       steps <- subset(StepsAverage, interval == obs$interval)$steps
   } else {
       steps <- obs$steps
   }
   fillNA <- c(fillNA, steps)
}
new_activity <- activity
new_activity$steps <- fillNA
StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(StepsTotal2$steps)
```

```
## [1] 10766.19
```

```r
median(StepsTotal2$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

