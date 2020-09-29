---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
if (!file.exists("activity_data.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
    destfile = "activity_data.zip")
  unzip("activity_data.zip")
}
activity <- read.csv("activity.csv",
                     colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day

```r
activity%>%
        group_by(date)%>%
        summarise(total_steps = sum(steps))%>%
        ggplot() +
        geom_histogram(aes(x=total_steps)) + 
        xlab("Total number of steps taken per day") + 
        labs(title = "Histogram")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Mean number of steps taken per day is

```r
total_steps_per_day <-  activity %>%
  group_by(date) %>%
  summarise(
    total_steps = sum(steps)
  ) %>%
  ungroup()
mean(total_steps_per_day$total_steps, na.rm=T)
```

```
## [1] 10766.19
```


Median number of steps taken per day is

```r
median(total_steps_per_day$total_steps, na.rm=T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
activity%>%
        group_by(interval)%>%
        summarise(total_steps_by_interval = sum(steps, na.rm=T))%>%
        ggplot() +
        geom_line(aes(x=interval, y=total_steps_by_interval)) + 
        ylab("Total number of steps taken per day") +
        xlab("Interval") +
        labs(title = "Time series plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

# 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
 activity%>%
        group_by(interval)%>%
        summarise(total_steps_by_interval = sum(steps, na.rm=T))%>%
        filter(total_steps_by_interval == max(total_steps_by_interval))%>%
        select(interval)
```

```
## # A tibble: 1 x 1
##   interval
##      <dbl>
## 1      835
```

## Imputing missing values

Total number of missing values in the dataset

```r
 sum(is.na(activity$steps))
```

```
## [1] 2304
```

Creating a new dataset that is equal to the original dataset but with the missing data filled in using the mean for that 5-minute interval.

```r
mean_activity_per_interval <- activity%>%
        group_by(interval)%>%
        summarise(mean_steps_by_interval = mean(steps, na.rm=T))
missing_activity <- activity%>%
        filter(is.na(steps))
for (i in 1:nrow(mean_activity_per_interval)){
        missing_activity[missing_activity$interval==as.numeric(mean_activity_per_interval[i, "interval"]), "steps"] <- 
                mean_activity_per_interval[i, "mean_steps_by_interval"]
}
complete_activity <- rbind(activity%>%
        filter(!is.na(steps)),
       missing_activity)
rm(missing_activity, mean_activity_per_interval, i)
```
# 7. Histogram of the total number of steps taken each day after missing values are imputed


```r
complete_activity%>%
        group_by(date)%>%
        summarise(total_steps = sum(steps))%>%
        ggplot() +
        geom_histogram(aes(x=total_steps)) + 
        xlab("Total number of steps taken per day after imputation") + 
        labs(title = "Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Mean number of steps taken per day is

```r
total_steps_per_day <-  complete_activity %>%
  group_by(date) %>%
  summarise(
    total_steps = sum(steps)
  ) %>%
  ungroup()
mean(total_steps_per_day$total_steps, na.rm=T)
```

```
## [1] 10766.19
```


Median number of steps taken per day is

```r
median(total_steps_per_day$total_steps, na.rm=T)
```

```
## [1] 10766.19
```
Imputation of missing values shifted mean and median to the mean of the intially observed data. This is expected since the means of respective intervals were used in the imputation. 


## Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
complete_activity <-  
        complete_activity%>%
        mutate(weekday = weekdays(date),
               weekday_weekend = ifelse(weekday%in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday"), "Weekday", "Weekend"))%>%
        select(- c(weekday))
```

Plotting

```r
mean_steps_complete_activity <-complete_activity %>%
        group_by(weekday_weekend, interval)%>%
        summarise(mean_steps = mean(steps))
lattice::xyplot(mean_steps ~ interval| weekday_weekend,
                data = mean_steps_complete_activity,
                type="l",
                ylab="Number of steps", xlab="Interval",
                layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
png("panelplot.png", height=480, width=480, units="px")
lattice::xyplot(mean_steps ~ interval| weekday_weekend,
                data = mean_steps_complete_activity,
                type="l",
                ylab="Number of steps", xlab="Interval",
                layout=c(1,2))
dev.off()
```

```
## quartz_off_screen 
##                 2
```
