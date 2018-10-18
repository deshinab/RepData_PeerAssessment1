---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## 1. Loading and preprocessing the data


```r
## 1.1 Load data
data <- read.csv(unzip("activity.zip"))

## 1.2 Omit NA values in the 'Step' variable. 

activity <- na.omit(data)
```
## 2. What is mean total number of steps taken per day?
        

```r
##load libraries required for these functions to work.
library(dplyr)
```

```r
#2.1 Calculate the total number of steps taken per day
stepsPerday <- group_by(activity, date)
ttlsteps <- summarize(stepsPerday, total = sum(steps)) 
head(ttlsteps, 3)
```

```
## # A tibble: 3 x 2
##   date       total
##   <fct>      <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
```
        

```r
#2.2 If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

##Get number of days with observations. 
hist(ttlsteps$total, main="Steps Per Day", 
     xlab="Total steps each day", col = "pink")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#2.3 Calculate and report the mean and median of the total number of steps taken per day
mean(ttlsteps$total)
```

```
## [1] 10766.19
```

```r
median(ttlsteps$total)
```

```
## [1] 10765
```

## 3. What is the average daily activity pattern?

```r
#3.1 Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
fivemin <- aggregate(steps ~ interval, activity, mean)
        plot(fivemin$interval, fivemin$steps, type='l', 
             main="Average steps across intervals during observation period", 
             xlab="Interval", 
             ylab="Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        # Find the row where the maximum number of steps are.
maxsteps<- which.max(fivemin$steps)
        # Find the data for that row. 
fivemin[maxsteps, ]
```

```
##     interval    steps
## 104      835 206.1698
```

## 4. Imputing missing values


```r
##4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
actwithNAs <- read.csv("activity.csv")
        sum(is.na(actwithNAs$steps))
```

```
## [1] 2304
```

```r
##4.2 Devise a strategy for filling in all of the missing values in the dataset.
        ## Replce NA values with the daily means. 
imputVal <- actwithNAs
        for (i in 1:nrow(imputVal)) {
          if (is.na(imputVal$steps[i])) {
                interval_value <- imputVal$interval[i]
                steps_value <- fivemin[
                        fivemin$interval == interval_value,]
                                imputVal$steps[i] <- steps_value$steps
          }
        }
head(imputVal, 3)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
```
##4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in. 

```r
##library needed for this function
library(data.table)
```

```r
##Create new dataset
new_data <- data.table(imputVal)
        head(new_data, 3)
```

```
##        steps       date interval
## 1: 1.7169811 2012-10-01        0
## 2: 0.3396226 2012-10-01        5
## 3: 0.1320755 2012-10-01       10
```

```r
##4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
        ## group data by day and calculate daily sum.
stepsPerday_imputed <- group_by(new_data, date)
SPD_imputed <- summarize(stepsPerday_imputed, total = sum(steps))
head(SPD_imputed, 3)
```

```
## # A tibble: 3 x 2
##   date        total
##   <fct>       <dbl>
## 1 2012-10-01 10766.
## 2 2012-10-02   126 
## 3 2012-10-03 11352
```

```r
        ##Histogram
hist(SPD_imputed$total, main="Steps Per Day whith NA values imputed with daily mean values", 
     xlab="Total steps each day", col = "purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
##Calcualte mean and meadian.
mean(SPD_imputed$total)
```

```
## [1] 10766.19
```

```r
median(SPD_imputed$total)
```

```
## [1] 10766.19
```

```r
#Alternatively...
summary(SPD_imputed$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
4.4.1 Do these values differ from the estimates from the first part of the assignment? 

The mean and median values are now the same.

4.4.2 What is the impact of imputing missing data on the estimates of the total daily number of steps?

Imputing missing data corrupts the integrity of the anlysis by changing the values of observations; thus manipulating the outcome. However, in this case, the difference was not large enough to change the histographic output.

## 5. Are there differences in activity patterns between weekdays and weekends?


```r
##5.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

## Add the day of the week for each respective date into oa new colum called "daytype"
imputVal["daytype"] <- weekdays(as.Date(imputVal$date))
head(imputVal, 3)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0  Monday
## 2 0.3396226 2012-10-01        5  Monday
## 3 0.1320755 2012-10-01       10  Monday
```

```r
## Make this variable a factor. 
daytype <- as.factor(imputVal$daytype)
## Specify days that are weekends and those that are weekdays. 
imputVal$daytype[imputVal$daytype  %in% c('Saturday','Sunday') ] <- "Weekend"
imputVal$daytype[imputVal$daytype != "Weekend"] <- "Weekday" 

## Make this variable a factor. 

head(imputVal, 3)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
```

```r
##5.2 Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


imputVal <- data.frame(imputVal)
# calculate average steps by interval across all days
stepsbyint <- aggregate(steps ~ interval + daytype, imputVal, mean)

##Library required for this function to work.
library(ggplot2)

# Create the plot
qplot(interval, steps, data = stepsbyint, 
        geom="line",
        xlab = "Interval", 
        ylab = "Number of steps", 
        main = "Weekday vs Weekend") +
  facet_wrap(~ daytype, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
