# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
##activity <- activity[!is.na(activity$steps),]
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
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
hist(steps_per_day, main = "Step per day distribution")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

* Mean and median of the total number of steps taken per day


```r
mean(sapply(split(activity$steps, activity$date), sum), na.rm = T)
```

```
## [1] 10766.19
```

```r
median(sort(sapply(split(activity$steps, activity$date), sum)))
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
x<-sapply(split(activity$steps, activity$interval), mean, na.rm = T)
length(x)
```

```
## [1] 288
```

```r
plot(unique(activity$interval), x, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
##plot(sapply(split(activity$steps, activity$interval), mean), activity$date)
max(x)
```

```
## [1] 206.1698
```

```r
names(x[x==max(x)])
```

```
## [1] "835"
```
## Imputing missing values

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

## Are there differences in activity patterns between weekdays and weekends?
