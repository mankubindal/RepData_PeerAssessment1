---
title: "Reproducible research: peer assignment 1"
output: html_document
---

##1. loading and pre processing the data

```r
unzip('activity.zip')
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
stepsdata <- read.csv("activity.csv")
```

##2. Histogram of the total number of steps taken each day


```r
library(ggplot2)
totalsteps <- tapply(stepsdata$steps, stepsdata$date, FUN=sum, na.rm=TRUE)
qplot(totalsteps, binwidth=500, xlab="total number of steps taken each day", ylab = "frequency of steps with bandwidth 500")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

##3. MEAN AND MEDIAN OF STEPS TAKEN EACH DAY


```r
mean(totalsteps,na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(totalsteps,na.rm = TRUE)
```

```
## [1] 10395
```
##4. Time series plot of the average number of steps taken

```r
agg<- aggregate(steps ~ interval, stepsdata,mean)
plot(agg$interval,agg$steps,type = "l", xlab = "interval", ylab = "steps taken",lwd = 2, col = "blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

##5. The 5-minute interval that, on average, contains the maximum number of steps

```r
agg[which.max(agg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

##6. Imputing missing values


```r
missingrows<- sum(is.na(stepsdata$steps))
missingrows
```

```
## [1] 2304
```

```r
fillvalue <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (agg[agg$interval==interval, "steps"])
        return(filled)
}
complete <- stepsdata
complete$steps <- mapply(fillvalue, complete$steps, complete$interval)

totalstepscomplete<- tapply(complete$steps, complete$interval, sum)
qplot(totalstepscomplete, binwidth=500, xlab="total number of steps taken each day after imputing", ylab = "frequency of steps with bandwidth 500 after imputing")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
mean(totalstepscomplete)
```

```
## [1] 2280.339
```

```r
median(totalstepscomplete)
```

```
## [1] 2080.906
```

##7. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
complete$day <- ifelse(as.POSIXlt(complete$date)$wday %in% c(0,6),'weekend','weekday')
aggimpute<- aggregate(steps ~ interval + day, complete, mean)
 ggplot(aggimpute, aes(interval, steps))+
geom_line() +
 facet_grid(.~day)+
 xlab("5-minute interval") + 
     ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

