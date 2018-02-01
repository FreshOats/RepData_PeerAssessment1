---
title: 'Reproducible Research: Peer Assessment 1'
author: "Justin R. Papreck"
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, dplyr is used throughout the processing, and is opened in the library. The 
data is read defining the characters "NA" for missing values, and then filtered out. 
The format of the date is changed from a character string to "Date"


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
activity <- read.csv("./repdata%2Fdata%2Factivity/activity.csv", header = TRUE, na.strings = "NA")
activity <- filter(activity, steps !="NA")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?
The data were grouped by date and summed. A histogram was used to show the frequency
distribution of the sums of steps by day. In this calculation, days with no collected
data were recorded as zero. The mean and median of the summed data were printed and
reflect the mean and median of the histogram. 




```r
activity_sums <- activity %>% group_by(date) %>% summarise(sumsteps = sum(steps), meansteps = mean(steps), medsteps = median(steps[steps>0]))
h <- hist(activity_sums$sumsteps)
```

![](PA1_template_files/figure-html/descriptives-1.png)<!-- -->

```r
print(h)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "activity_sums$sumsteps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
png(file = "Hist1.png", width = 480, height = 480, units = "px")
print(h)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "activity_sums$sumsteps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.off()
```

```
## png 
##   2
```

```r
print(mean(activity_sums$sumsteps))
```

```
## [1] 10766.19
```

```r
print(median(activity_sums$sumsteps))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
In these analyses, the data were grouped and observed by interval time throughout
the day, where interval 0 represents 12am, and data were collected every 5 minutes 
following until 11:55pm. The maximum mean number of steps in any given interval 
was printed along with the corresponding interval number. 



```r
int_times <- activity %>% group_by(interval) %>% summarise(meanints = mean(steps), medianints = median(steps))
g <- ggplot(int_times, aes(interval, meanints))
g <- g + geom_line() + labs(x = "Interval", y = "Mean Steps")
print(g)
```

![](PA1_template_files/figure-html/itervals-1.png)<!-- -->

```r
png(file = "int_times.png", width = 480, height = 480, units = "px")
print(g)
dev.off()
```

```
## png 
##   2
```

```r
max_interval <- filter(int_times, meanints == max(int_times$meanints))
print(max_interval)
```

```
## # A tibble: 1 x 3
##   interval meanints medianints
##      <int>    <dbl>      <int>
## 1      835 206.1698         19
```

## Imputing missing values
The original data is brought up again, but this time the NA values are imputed 
rather than ignored. The number of rows with NA values recorded is first shown.
The NA values will be replaced with the Median value of each interval.  


```r
Imputer <- read.csv("./repdata%2Fdata%2Factivity/activity.csv", header = TRUE, na.strings = "NA")
print(sum(is.na(Imputer)))
```

```
## [1] 2304
```

```r
Imputer <- Imputer %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), median(steps, na.rm = TRUE), steps))
print(sum(is.na(Imputer)))
```

```
## [1] 0
```

The new dataset "Imputer" is a copy of the original data, which then uses the median
of each interval group to replace the number of steps for all NA values. Prior to 
this, the number of NA values was printed, and then confirmed as '0' following the 
data impute. 


```r
Imputer_D <- Imputer %>% group_by(date) %>% summarise(Imp_steps = sum(steps))
h <- hist(Imputer_D$Imp_steps)
```

![](PA1_template_files/figure-html/imputed descriptives-1.png)<!-- -->

```r
print(h)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1] 13 12 28  6  2
## 
## $density
## [1] 4.262295e-05 3.934426e-05 9.180328e-05 1.967213e-05 6.557377e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "Imputer_D$Imp_steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
png(file = "Hist2.png", width = 480, height = 480, units = "px")
print(h)
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1] 13 12 28  6  2
## 
## $density
## [1] 4.262295e-05 3.934426e-05 9.180328e-05 1.967213e-05 6.557377e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "Imputer_D$Imp_steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
dev.off()
```

```
## png 
##   2
```

```r
print(mean(Imputer_D$Imp_steps))
```

```
## [1] 9503.869
```

```r
print(median(Imputer_D$Imp_steps))
```

```
## [1] 10395
```

These Values differ from those obtained when not imputing the data. In both cases 
of mean and median, the values obtained after imputing data were lower than those
where all NA values were ignored. The impact suggests that the number of daily 
steps is lower than what was originally calculated, which could have been impacted 
by a few days with extremely high values. 

## Are there differences in activity patterns between weekdays and weekends?
Initially, we can add a new column to the Imputer file adding the weekday of each 
date. Then the weekdays and weekends are subsetted into their own dataframes and 
then re-bound into a dataframe called Week, which is then called up on to plot. 


```r
Imputer$date <- as.Date(Imputer$date)
Imputer <- mutate(Imputer, weekday = weekdays(date))
weekdays <- subset(Imputer, Imputer$weekday != "Saturday" & Imputer$weekday !="Sunday")
weekend <- subset(Imputer, Imputer$weekday == "Saturday" | Imputer$weekday =="Sunday")

int_end <- weekend %>% group_by(interval) %>% summarise(mean_ints = mean(steps), median_ints = median(steps))
int_end <- mutate(int_end, type = "Weekend")
int_week <- weekdays %>% group_by(interval) %>% summarise(mean_ints = mean(steps), median_ints = median(steps))
int_week <- mutate(int_week, type = "Weekdays")
Week <- rbind(int_week, int_end)

g <- ggplot(Week, aes(x = interval, y = mean_ints))
g <- g + geom_line() + labs(x = "Interval", y = "Mean Steps") + facet_wrap(~type, nrow=2)
print(g)
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->

```r
png(file = "Week.png", width = 480, height = 480, units = "px")
print(g)
dev.off()
```

```
## png 
##   2
```

As the data shows, there appears to be a difference between the weekday and weekend distributions. While the both weekday and weekend plots show the same general interval spike in the morning, the spike on the weekdays has a higher mean, though, throughout the weekend, the mean number of steps appears higher.  
