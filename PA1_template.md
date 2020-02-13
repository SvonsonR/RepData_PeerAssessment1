A. Code for reading in the dataset and/or processing the data
=============================================================

    if(!file.exists('activity.csv')){
        unzip('activity.zip')
    }
    ActivityData <- read.csv('activity.csv')
    summary(ActivityData)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

B. What is mean total number of steps taken per day?
====================================================

1. Calculate the total number of steps taken per day
----------------------------------------------------

    StepsPerDay <- aggregate(steps ~ date, ActivityData, sum, na.rm=TRUE)

2. Make a histogram of the total number of steps taken each day
---------------------------------------------------------------

    library(ggplot2)
    qplot(StepsPerDay$steps, xlab='Total steps per day', ylab='Frequency', binwidth=400)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day
--------------------------------------------------------------------------------------

    summary(StepsPerDay$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    8841   10765   10766   13294   21194

C. What is the average daily activity pattern?
==============================================

1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
------------------------------------------------------------------------------------------------------------------------------------------------------

    StepsPerInterval<-aggregate(steps ~ interval, data = ActivityData, mean, na.rm=TRUE)
    plot(steps ~ interval, data = StepsPerInterval, type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
----------------------------------------------------------------------------------------------------------------

    print(IntervalMaxSteps <- StepsPerInterval[which.max(StepsPerInterval$steps),]$interval)

    ## [1] 835

D. Imputing missing values
==========================

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
------------------------------------------------------------------------------------------------------------------

    summary(ActivityData)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean(/median) for (that day, or the mean for) that 5-minute interval, etc.
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    MeanStepsPerInterval<-function(interval){
        StepsPerInterval[StepsPerInterval$interval==interval,]$steps
    }

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
--------------------------------------------------------------------------------------------------

    ActivityDataNoNA<-ActivityData
    for(i in 1:nrow(ActivityDataNoNA)){
        if(is.na(ActivityDataNoNA[i,]$steps)){
            ActivityDataNoNA[i,]$steps <- MeanStepsPerInterval(ActivityDataNoNA[i,]$interval)
        }
    }

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    StepsPerDayNoNA <- aggregate(steps ~ date, data = ActivityDataNoNA, sum)
    hist(StepsPerDayNoNA$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    print(MeanStepsPerDayNoNA <- mean(StepsPerDayNoNA$steps))

    ## [1] 10766.19

    print(MedianStepsPerDayNoNA <- median(StepsPerDayNoNA$steps))

    ## [1] 10766.19

-   The mean stays the same after the replacements of NAs  
-   The median changed about ~ 0.1% of the original value

E. Are there differences in activity patterns between weekdays and weekends?
============================================================================

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
----------------------------------------------------------------------------------------------------------------------------------------------------

    # My system is in German, so i need to use the German words for Saturday and Sunday
    ActivityDataNoNA$day <- ifelse(weekdays(as.Date(ActivityDataNoNA$date)) == "Samstag" | 
                                     weekdays(as.Date(ActivityDataNoNA$date)) == "Sonntag", "weekend", "weekday")

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    StepsPerDay <- aggregate(ActivityDataNoNA$steps ~ ActivityDataNoNA$interval + ActivityDataNoNA$day, ActivityDataNoNA, mean)
    names(StepsPerDay) <- c("interval", "day", "steps")
    library(lattice)
    xyplot(steps ~ interval | day, StepsPerDay, type = "l", layout = c(1, 2), 
        xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
