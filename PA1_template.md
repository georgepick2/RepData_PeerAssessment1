# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


Load the data in variable "data" and convert the dates.

```r
data = read.csv("activity.csv")
data = mutate(data, date=as.Date(date))
```

## What is mean total number of steps taken per day?

```r
stepsperday = ddply(data, .(date), summarize, totalsteps=sum(steps))
hist(stepsperday$totalsteps, 100, main="Histogram of total number of steps each day", xlab="Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean number of steps taken per day is 
10766.2 and the median number of steps each day is 10765.


## What is the average daily activity pattern?

```r
stepsperinterval = ddply(data, .(interval), summarize, averagesteps=mean(steps,na.rm=TRUE))
with(stepsperinterval,plot(interval, averagesteps, type="l", main="Average number of steps per interval", xlab="Interval", ylab="Number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The maximum value is achieved at:

```r
stepsperinterval[which.max(stepsperinterval$averagesteps),]
```

```
##     interval averagesteps
## 104      835     206.1698
```

## Imputing missing values
Total number of missing values in the dataset is 2304.

The strategy for imputing missing data is to replace each NA with the average for the respective interval accross all days.


```r
data2 = data
for (i in which(is.na(data$steps))) {
    data2[i,"steps"] <- stepsperinterval[which(stepsperinterval$interval==data[i,"interval"]),"averagesteps"]}
```


```r
stepsperday2 = ddply(data2, .(date), summarize, totalsteps=sum(steps))
hist(stepsperday2$totalsteps, 100, main="Histogram of total number of steps each day w/o NAs", 
     xlab="Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The new mean number of steps taken per day is 
10766.2 and the new median number of steps each day is 10766.2.


## Are there differences in activity patterns between weekdays and weekends?

```r
weekend = function(x) if(x=="Saturday" || x=="Sunday") return("weekend") else return("weekday")
data2 = mutate(data2, day=as.factor(sapply(weekdays(data2$date), weekend)))
stepsperinterval3 = ddply(data2, .(interval,day), summarize, averagesteps=mean(steps,na.rm=TRUE))
xyplot(averagesteps ~ interval | day, data=stepsperinterval3, type="l", xlab="Interval", ylab="Number of steps", layout= c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
