# Reproducible Research: Peer Assessment 1
Vadim Katsemba  
September 7, 2016  

##LOADING AND PREPROCESSING THE DATA

##Read the data file

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.2
```

```r
library(rmarkdown)
```

```
## Warning: package 'rmarkdown' was built under R version 3.2.2
```

```r
echo = TRUE
activity <- read.csv("C:\\Users\\Vadim Katsemba\\Documents\\activity.csv")
```

##Transforming the variables

```r
date <- as.Date(activity$date, format = "%Y-%m-%d")
interval <- factor(activity$interval)
```

##WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?

##Remove all the NA values

```r
na <- is.na(as.character(activity$steps))
newdata <- activity[!na,]
```

##Calcuate the total number of steps, the mean, median and represent them with a histogram 

```r
dsteps <- aggregate(steps ~ date, data = newdata, sum)
hist(as.numeric(dsteps$steps), breaks = 25, col = "blue", xlab = "Daily Steps", main = "Total steps taken each day")
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png?raw=true) 

```r
mean(dsteps$steps)
```

```
## [1] 10766.19
```

```r
median(dsteps$steps)
```

```
## [1] 10765
```

##WHAT IS THE DAILY AVERAGE ACTIVITY PATTERN?

##Compute the average number of steps taken and plot a time series plot

```r
echo = TRUE
int <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(int ~ unique(activity$interval), type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png?raw=true) 

##Maximum number of steps

```r
int[which.max(int)]
```

```
##      835 
## 206.1698
```

##The 835th interval is where the maximum number of steps took place

##IMPUTING MISSING VALUES

##Total number of missing values

```r
sum(is.na(as.character(activity$steps)))
```

```
## [1] 2304
```

##Filling in the missing values

```r
replacena <- which(is.na(as.character(activity$steps)))
intsteps <- aggregate(newdata$steps, by=list(interval=newdata$interval), FUN=mean)
colnames(intsteps) <- c("interval", "averagesteps")
newdata1 <- activity
newdata1[replacena, ]$steps <- unlist(lapply(replacena,FUN=function(replacena) intsteps[activity[replacena,]$interval==intsteps$interval,]$averagesteps))
```

##Creating a new data set with the missing data filled in and making a histogram

```r
sum(is.na(as.character(newdata1$steps)))
```

```
## [1] 0
```

```r
fullsteps <- aggregate(steps ~ date, data = newdata1, sum)
colnames(fullsteps) <- c("date","steps")
hist(as.numeric(fullsteps$steps), breaks = 25, col = "green", xlab = "# of Steps", main = "Total Number of Steps Taken Each Day")
```

![](PA1_files/figure-html/unnamed-chunk-9-1.png?raw=true) 

##Mean and median of the total number of steps

```r
mean(fullsteps$steps)
```

```
## [1] 10766.19
```

```r
median(fullsteps$steps)
```

```
## [1] 10766.19
```

##The mean is still the same, whereas the median is now greater by 1 step.

##ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?

##Create weekday and weekend factor variables

```r
weekdaysvar <- weekdays(as.Date(newdata1$date))
newdata1$daytype <- sapply(weekdaysvar, function(sgn) {
  if (sgn %in% c("Saturday", "Sunday")) {
    return("weekend")
  } else {
    return("weekday")
  }
})  
```

##Grouping day for interval, type of day and average steps per group

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(magrittr)
```

```
## Warning: package 'magrittr' was built under R version 3.2.5
```

```r
dtintsteps <- newdata1 %>% group_by(interval, daytype) %>%
  summarise(avsteps = mean(steps))
```

##Plot the time series plot of the interval, steps taken and average across weekdays

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.2
```

```r
xyplot(avsteps ~ interval | daytype, data = dtintsteps, type = "l", layout = c(1,2))
```

![](PA1_files/figure-html/unnamed-chunk-13-1.png?raw=true) 

