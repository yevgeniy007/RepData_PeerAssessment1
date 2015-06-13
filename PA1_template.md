---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



First, I load data from the data file: *"activity.csv"* and remove the rows with missing data **NA**. In addition, I transform the column *date* to Date format. 

The dataset typically has  several rows for the same day, and therefore grouping rows by day is required to calculate the average number of steps per day.


```r
rm(list=ls())
mydata=read.csv("activity.csv")

library(dplyr)

# Data transformation and grouping are combined into one command
mydata_byday <-group_by(mutate(na.omit(mydata), date=as.Date(date)), date)
mydata_daily<-summarize(mydata_byday, steps_daily=sum(steps))
```

 
## What is mean total number of steps taken per day?

 
Then, we can calculate the mean and the median number of steps per day:


```r
mean(mydata_daily$steps_daily)
```

```
## [1] 10766.19
```

```r
median(mydata_daily$steps_daily)
```

```
## [1] 10765
```


**Please note** that mean and the calculations of the mean and median values do not include the days for which there are no records of steps (*steps=0*). In this case the median and the mean are similar, which shows that the distribution of steps per day is not skewed. 

Histogram:


```r
hist(mydata_daily$steps_daily, breaks=60, main="The number of steps taken daily (NA removed)", ylab="Frequency, days in dataset", xlab="Steps per day", col="blue", border ="white")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

This diagram shows that there are records (days) for which there are not steps recorded. There are days for which the number of steps is greater than 20,000. It is estuimated that roughly the number of steps in a mile is 2000, therefore these records correspond to long journeys of more than 10 miles.  


## What is the average daily activity pattern?


The pattern of daily activity can be illustrated by a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
mydata_byinterval <-group_by(na.omit(mydata), interval)
library(ggplot2)
data4timeseries<-summarize(mydata_byinterval, steps=mean(steps))
ggplot(data4timeseries, aes(x=interval, y=steps))+geom_line() + labs(x="5-min interval", y="Steps" ) + geom_point(color="steelblue", alpha = 1/2) + geom_line(colour = "steelblue", size = 1/3) + labs(title="Time series for 5-min intervals")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Further, the maximum number of steps within a 5-minutes interval averaged across days is: 


```r
round(max(data4timeseries$steps),1)
```

```
## [1] 206.2
```

The time interval corresponding to this value: 



```r
maxstepsinterval<-filter(data4timeseries, steps==max(data4timeseries$steps))
maxstepsinterval$interval
```

```
## [1] 835
```

## Imputing missing values


The presence of missing days (values = **NA**) may introduce bias into some calculations or summaries of the data.
The total number of rows with missing data: 



In order to impute the missing deta, the following approach was taken:

- The value of steps for a given row in a dataset will be assumed to be equal to the average across all days corresponding to the interval for which the data alement is missing. This is done by merging the dataset with a dataset with averages steps per time interval. 

1. Create an NA-free dataset 
2. Create a dataset by selectung missing data 
3. Merge data in the dataset with missing data with the dataset with the averages steps
4. Replace the missing data with the averages for specific interval 
5. Harmonize the column names
6. Merge datasets into an NA-free dataset 



```r
mydata_no_NA <- filter(mydata, !is.na(steps))  # step 1 
mydata_NA <- filter(mydata, is.na(steps))     # step 2
mydata_NA_merged<-merge(mydata_NA, data4timeseries, by = "interval") # step 3 
mydata_NA<-mutate(mydata_NA_merged, steps.x=steps.y) # step 4
mydata_NA<-select((mutate(mydata_NA, steps=steps.x)), interval, date, steps) # step 5
mydata_new<-rbind(mydata_no_NA, mydata_NA) # step 6 
```

#### Reanalyzing the dataset with missing data imputed 

Calculating the new mean and median: 


```r
## Data transformation and grouping are combined into one command

mydata_byday <-group_by(mutate(mydata_new, date=as.Date(date)), date)
mydata_daily<-summarize(mydata_byday, steps_daily=sum(steps))

# Mean
mean(mydata_daily$steps_daily)
```

```
## [1] 10766.19
```

```r
# Median 
median(mydata_daily$steps_daily)
```

```
## [1] 10766.19
```


#### Histogram:


```r
hist(mydata_daily$steps_daily, breaks=60, main="The number of steps taken daily", ylab="Frequency, days in dataset", xlab="Steps per day", col="green", border ="white")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


#### Time series


```r
mydata_byinterval <-group_by(mydata_new, interval)

library(ggplot2)

data4timeseries<-summarize(mydata_byinterval, steps=mean(steps))
ggplot(data4timeseries, aes(x=interval, y=steps))+geom_line() + labs(x="5-min interval", y="Steps" ) + geom_point(color="red", alpha = 1/2) + geom_line(colour = "red", size = 1/3) + labs(title="Time series for 5-min intervals")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


## Are there differences in activity patterns between weekdays and weekends?



### End of assignment  

