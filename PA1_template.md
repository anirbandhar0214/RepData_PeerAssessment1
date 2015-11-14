# Reproducible Research Peer Assessment 1
## Loading and preprocessing the data
Show any code that is needed to
Load the data (i.e. read.csv()).
Process/transform the data (if necessary) into a format suitable for your analysis.


```r
df <- read.csv("activity.csv",as.is=TRUE)
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
Make a histogram of the total number of steps taken each day.
Calculate and report the mean and median total number of steps taken per day.

```r
# Ignore missing values
dfNoNA <- na.omit(df)

# aggregate steps
table_agg_date_steps <- aggregate(steps ~ date, dfNoNA, sum)

# create histogram
hist(table_agg_date_steps$steps, col=1, main="Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# get mean and median
mean(table_agg_date_steps$steps)
```

```
## [1] 10766.19
```

```r
median(table_agg_date_steps$steps)
```

```
## [1] 10765
```
The mean and median total number of steps per day are 10766.19 and 10765 steps respectively.
## What is the average daily activity pattern?
Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# aggregate steps as interval 
table_agg_interval_steps <- aggregate(steps ~ interval, df2, mean)

# generate the line plot
plot(table_agg_interval_steps$interval, table_agg_interval_steps$steps, type='l', col=1, main="Average number of steps averaged over all days", xlab="Interval",  ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# row id of maximum average number of steps in an interval
max_ave_steps_row_id <- which.max(table_agg_interval_steps$steps)

# get the interval with maximum average number of steps in an interval
table_agg_interval_steps [max_ave_steps_row_id, ]
```

```
##     interval    steps
## 104      835 206.1698
```
The interval 835 has the maximum average number of 206 steps
## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# get rows with missing values 
df_Missing_NA <- df[!complete.cases(df),]

# number of rows
nrow(df_Missing_NA)
```

```
## [1] 2304
```
The total number of rows with NA's is 2304 as shown above.

```r
# perform the imputation
for (i in 1:nrow(df)){
  if (is.na(df$steps[i])){
    interval_val <- df$interval[i]
    row_id <- which(table_agg_interval_steps$interval == interval_val)
    steps_val <- table_agg_interval_steps$steps[row_id]
    df$steps[i] <- steps_val
  }
}

# aggregate steps as per date to get total number of steps in a day
table_date_steps_imputed <- aggregate(steps ~ date, df, sum)

# create histogram of total number of steps in a day
hist(table_date_steps_imputed$steps, col=1, main="(Imputed) Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
# get mean and median of total number of steps per day
mean(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(table_date_steps_imputed$steps)
```

```
## [1] 10766.19
```

```r
# get mean and median of total number of steps per day for data with NA's removed
mean(table_agg_date_steps$steps)
```

```
## [1] 10766.19
```

```r
median(table_agg_date_steps$steps)
```

```
## [1] 10765
```
After data imputation, the mean and median total number of steps per day are 10766.19 and 10765 steps respectively.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
# convert date from string to Date class
df$date <- as.Date(df$date, "%Y-%m-%d")

# add day of the week 
df$day <- weekdays(df$date)

# add day type and initialize to weekday
df$day_type <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(df)){
  if (df$day[i] == "Saturday" || df$day[i] == "Sunday"){
    df$day_type[i] <- "weekend"
  }
}

# convert day_time from character to factor
df$day_type <- as.factor(df$day_type)

# aggregate steps as interval
table_agg_interval_steps_imputed <- aggregate(steps ~ interval+day_type, df, mean)

# make the panel plot for weekdays and weekends
library(ggplot2)
qplot(interval, steps, data=table_agg_interval_steps_imputed, geom=c("line"), xlab="Interval", ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

End of First Assignment
