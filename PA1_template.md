# Reproducible Research: Peer Assessment 1
### Loading and preprocessing the data

```r
    # opts_chunk$set(echo=TRUE, results='asis')
    if(!file.exists('activity.csv')){
    unzip('activity.zip')
    }
    activity <- read.csv('activity.csv')
```
### Process/transform the data (if necessary) into a format suitable for your analysis
#### Create a date.time column that combines the date and interval columns.

```r
    time <- formatC(activity$interval / 100, 2, format='f')
    activity$date.time <- as.POSIXct(paste(activity$date, time),
                                 format='%Y-%m-%d %H.%M',
                                 tz='GMT')
```
#### convert all of the dates to be for today

```r
  activity$time <- format(activity$date.time, format='%H:%M:%S')
  activity$time <- as.POSIXct(activity$time, format='%H:%M:%S')
```
#### What is mean total number of steps taken per day?
##### calculate the mean number of steps for each day

```r
  total.steps <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```
##### The mean and median for the total steps per day

```r
    mean(total.steps)
```

```
## [1] 9354.23
```

```r
    median(total.steps)
```

```
## [1] 10395
```
##### The distribution of total number of steps per day with a histogram

```r
    library(ggplot2)
    qplot(total.steps, xlab='Total steps', ylab='Frequency')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
#### What is the average daily activity pattern?

##### the mean steps for each five minute interval, and then put it in a data frame
```

```r
    mean.steps <- tapply(activity$steps, activity$time, mean, na.rm=TRUE)
    daily.pattern <- data.frame(time=as.POSIXct(names(mean.steps)),mean.steps=mean.steps)
```
##### Time series plot for the mean steps.

```r
    library(scales)
    ggplot(daily.pattern, aes(time, mean.steps)) + 
    geom_line() +
    xlab('Time of day') +
    ylab('Mean number of steps') +
    scale_x_datetime(labels=date_format(format='%H:%M'))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
##### Five minute interval has the highest mean number of steps

```r
  most <- which.max(daily.pattern$mean.steps)
  format(daily.pattern[most,'time'], format='%H:%M')
```

```
## [1] "08:35"
```
#### Inputing missing values

```r
  summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
##### Five-minute interval for the entire dataset

```r
  library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
  activity.imputed <- activity
  activity.imputed$steps <- with(activity.imputed, impute(steps, mean))
```
##### compare the mean and median steps for each day between the original data set and the imputed data set

```r
    total.steps.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
    mean(total.steps)
```

```
## [1] 9354.23
```

```r
    mean(total.steps.imputed)
```

```
## [1] 10766.19
```

```r
    median(total.steps)
```

```
## [1] 10395
```

```r
    median(total.steps.imputed)
```

```
## [1] 10766.19
```
##### Histogram of the imputed dataset

```r
    qplot(total.steps.imputed, xlab='Total steps', ylab='Frequency')
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 


#### Are there differences in activity patterns between weekdays and weekends?

```r
    day.type <- function(date) {
    if (weekdays(date) %in% c('Saturday', 'Sunday')) {
        return('weekend')
    } else {
        return('weekday')
    }
}

  day.types <- sapply(activity.imputed$date.time, day.type)
  activity.imputed$day.type <- as.factor(day.types)
```
##### Create a dataframe that holds the mean steps for weekdays and weekends.

```r
    mean.steps <- tapply(activity.imputed$steps, 
                     interaction(activity.imputed$time,
                                 activity.imputed$day.type),
                     mean, na.rm=TRUE)
    day.type.pattern <- data.frame(time=as.POSIXct(names(mean.steps)),
                               mean.steps=mean.steps,
                               day.type=as.factor(c(rep('weekday', 288),
                                                   rep('weekend', 288))))                                                   
```
##### compare the patterns between weekdays and weekends.

```r
    ggplot(day.type.pattern, aes(time, mean.steps)) + 
    geom_line() +
    xlab('Time of day') +
    ylab('Mean number of steps') +
    scale_x_datetime(labels=date_format(format='%H:%M')) +
    facet_grid(. ~ day.type)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 




