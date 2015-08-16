# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Loading the data:


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

* Total number of steps taken per day


```r
steps_per_day<-sapply(split(activity$steps, activity$date), sum)
steps_per_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

* Histogram of the total number of steps taken each day


```r
steps_per_day<-steps_per_day[complete.cases(steps_per_day)]
hist(steps_per_day, ylim=c(0,35), xlab =  "Steps per days", main = "Steps per day distribution")
text(mean(steps_per_day)-4000,33,paste("Mean = ",round(mean(steps_per_day),0)," -->"), col="blue" )
text(median(steps_per_day)+4300,33,paste("<-- Median = ",round(median(steps_per_day))), col="red" )
abline(v=mean(steps_per_day), col="blue")
abline(v=median(sort(steps_per_day)), col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

* Mean and median of the total number of steps taken per day


```r
mean(steps_per_day)
```

```
## [1] 10766.19
```

```r
median(sort(steps_per_day))
```

```
## [1] 10765
```

As you can see, the mean and median are practically equal.

## What is the average daily activity pattern?

* Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval_means<-sapply(split(activity$steps, activity$interval), mean, na.rm = T)
plot(unique(activity$interval), steps_interval_means, 
     xlab="Intervals", ylab="Step", 
     main="Averaged Steps per Intervals", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The maximun number of steps on average is


```r
max(steps_interval_means)
```

```
## [1] 206.1698
```

that belong to the interval:


```r
 names(steps_interval_means[steps_interval_means==max(steps_interval_means)])
```

```
## [1] "835"
```

## Imputing missing values

* The total number of missing values in the dataset is:


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

* Filling missing values with the mean of corresponding interval, in a new dataset:


```r
activity1<-activity
missing_values <- !complete.cases(activity1)
activity_na <-activity1[missing_values,]
for(i in activity_na$interval){  
 activity_na[activity_na$interval==i,]$steps=steps_interval_means[as.character(i)]  
}
#New dataset activity1 without missing values
activity1[missing_values,]$steps = round(activity_na$steps,2)
```

* Total number of steps taken per day:


```r
steps_per_day1<-sapply(split(activity1$steps, activity1$date), sum)
```

* Histogram of the total number of steps taken each day


```r
hist(steps_per_day1, ylim=c(0,40), xlab =  "Steps per days", main = "Steps per day distribution")
text(mean(steps_per_day1)-4000,38,paste("Mean = ",round(mean(steps_per_day1),0)," -->"), col="blue" )
text(median(steps_per_day1)+4300,38,paste("<-- Median = ",round(median(steps_per_day1))), col="red" )
abline(v=mean(steps_per_day1), col="blue")
abline(v=median(sort(steps_per_day1)), col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

* Mean and median of the total number of steps taken per day with filled missing values:


```r
mean(steps_per_day1)
```

```
## [1] 10766.18
```

```r
median(sort(steps_per_day1))
```

```
## [1] 10766.13
```

Although we replaced the missing values with the mean of the steps per interval and as it can be seen in the plot, where the frecuency only had a little change at the portion 10000-15000, the differences between mean and median with and without missing values is very small.

So, we can conclude that there are not significant differences between both dataset.

## Are there differences in activity patterns between weekdays and weekends?


```r
wd<-c("Monday"="weekday","Tuesday"="weekday","Wednesday"="weekday",
      "Thursday"="weekday","Friday"="weekday", "Saturday"="weekend" ,"Sunday"="weekend")
days <- weekdays(  as.Date(activity1$date))
activity1$day_type=factor(wd[days])

library(ggplot2)
steps_means <- aggregate(activity1$steps, 
                         by=list(interval=activity1$interval, 
                                 day_type=activity1$day_type),
                         mean)
qplot( interval, x, xlab="Intervals", ylab="Steps", 
       data=steps_means,colours=c("blue", "red"),  
       geom="line")+ facet_wrap(~day_type, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

In above plot we can see significant averages changes in the patterns.

In weekends the quantity of steps, between intervals 500 and 1000, experiment a great reduction, from approximately 76 to 45 steps per interval,   

```r
#steps for 500 to 1000 intervals
#weekdays average steps
mean(steps_means[steps_means$day_type =="weekday" 
                 & steps_means$interval>=500 
                 & steps_means$interval<=1000, ]$x)
```

```
## [1] 76.01571
```

```r
#weekends average steps
mean(steps_means[steps_means$day_type =="weekend" 
                 & steps_means$interval>=500 
                 & steps_means$interval<=1000, ]$x)
```

```
## [1] 45.64451
```

and an increment between intervals 1000 and 2000 from approximately 43 to 70 steps per interval.


```r
#steps for 1000 to 2000 intervals
#weekdays average steps
mean(steps_means[steps_means$day_type =="weekday" 
                 & steps_means$interval>=1000 
                 & steps_means$interval<=2000, ]$x)
```

```
## [1] 43.43099
```

```r
#weekends average steps
mean(steps_means[steps_means$day_type =="weekend" 
                 & steps_means$interval>=1000 
                 & steps_means$interval<=2000, ]$x)
```

```
## [1] 70.84258
```



