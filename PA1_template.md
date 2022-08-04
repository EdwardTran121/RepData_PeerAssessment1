---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading the needed library

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.6     v purrr   0.3.4
## v tibble  3.1.7     v dplyr   1.0.9
## v tidyr   1.2.0     v stringr 1.4.0
## v readr   2.1.2     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(knitr)
```

## Loading and preprocessing the data
1. Load and assign the file to activity. If the file already existed, the code would not run.

```r
if(!file.exists('activity.csv')){
  unzip(activity.zip)
}
activity <- read.csv('activity.csv')
```
2. Investigate the data
Data contains 17568 observations and 3 variables. The variables includes steps, date and interval (Details are mentioned in [README.md](https://github.com/EdwardTran121/RepData_PeerAssessment1/blob/master/README.md) file).

```r
dim(activity)
```

```
## [1] 17568     3
```
The output below shows a glance of the activity data.

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
There are some NAs in steps. Filter the NAs into filtered_activity.

```r
filtered_activity <- activity %>% 
  filter(!is.na(steps)) %>% 
  mutate(date = as.Date(date))
```
The number of remained observations is 15264. Date is also changed format to date.
Take a look at filtered_activity.

```r
head(filtered_activity)
```

```
##   steps       date interval
## 1     0 2012-10-02        0
## 2     0 2012-10-02        5
## 3     0 2012-10-02       10
## 4     0 2012-10-02       15
## 5     0 2012-10-02       20
## 6     0 2012-10-02       25
```

```r
dim(filtered_activity)
```

```
## [1] 15264     3
```

## What is mean total number of steps taken per day?
Group the data by date and assign it to day_activity.

```r
day_activity <- filtered_activity %>% 
  group_by(date) %>% 
  summarise(steps_per_day = sum(steps)) 
head(day_activity)
```

```
## # A tibble: 6 x 2
##   date       steps_per_day
##   <date>             <int>
## 1 2012-10-02           126
## 2 2012-10-03         11352
## 3 2012-10-04         12116
## 4 2012-10-05         13294
## 5 2012-10-06         15420
## 6 2012-10-07         11015
```
There are 53 days in data.

```r
dim(day_activity)
```

```
## [1] 53  2
```

1. The histogram of for number of steps per day is shown below. The distribution of the data seems to be normal distribution.

```r
ggplot(day_activity, aes(steps_per_day)) +
  geom_histogram() +
  labs(title = "Distribution of Steps per Day") +
  xlab("Steps per day") +
  ylab('Number of day') +
  theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

2. The average steps per day is 1.0766189\times 10^{4} and median is 10765, which are summarized below.

```r
summary(day_activity$steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?
Group the data by interval per day and remove NA

```r
interval_activity <- filtered_activity %>% 
  group_by(interval) %>% 
  summarise(steps_per_interval = mean(steps)) 
head(interval_activity)
```

```
## # A tibble: 6 x 2
##   interval steps_per_interval
##      <int>              <dbl>
## 1        0             1.72  
## 2        5             0.340 
## 3       10             0.132 
## 4       15             0.151 
## 5       20             0.0755
## 6       25             2.09
```
1. The time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is shown below.

```r
ggplot(interval_activity, aes(interval, steps_per_interval)) +
  geom_line() +
  labs(title = "Average number of steps taken acroos all days") +
  xlab("5-minute interval") +
  ylab('Average number of steps') +
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
2. On average across all the days in the data set, the 5-minute interval contains the maximum number of steps is 835

```r
interval_activity %>% 
  arrange(desc(steps_per_interval)) %>% 
  head()
```

```
## # A tibble: 6 x 2
##   interval steps_per_interval
##      <int>              <dbl>
## 1      835               206.
## 2      840               196.
## 3      850               183.
## 4      845               180.
## 5      830               177.
## 6      820               171.
```


## Imputing missing values
1. The total number of missing values in the dataset is 2304

```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```
Investigate the missing value data.

```r
activity %>% 
  filter(is.na(steps)) %>% 
  summarise_each(funs(year = year, month = month), date) %>% 
  table()
```

```
## Warning: `summarise_each_()` was deprecated in dplyr 0.7.0.
## Please use `across()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```
##       month
## year     10   11
##   2012  576 1728
```

2. Using imputing mean to assign mean of the same interval the the missing value.

```r
imputed_activity <- activity %>% 
  mutate(date = as.Date(date))
for (i in 1:nrow(activity)){
  imputed_activity[i,1][is.na(imputed_activity[i,1])] <- 
    interval_activity$steps_per_interval[interval_activity$interval == imputed_activity[i,3]]
}
head(imputed_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
3. Check the NA in the imputed variable

```r
sapply(imputed_activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##        0        0        0
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. The **mean** is not changed while **median** is changed that equal to **mean** from the estimates from the first part of the assignment.
Calculate steps taken each day

```r
day_imputed_activity <- imputed_activity %>% 
  group_by(date) %>% 
  summarise(steps_per_day = sum(steps)) 
head(day_imputed_activity)
```

```
## # A tibble: 6 x 2
##   date       steps_per_day
##   <date>             <dbl>
## 1 2012-10-01        10766.
## 2 2012-10-02          126 
## 3 2012-10-03        11352 
## 4 2012-10-04        12116 
## 5 2012-10-05        13294 
## 6 2012-10-06        15420
```
Histogram of total number of steps taken each day in filled NAs data set.

```r
ggplot(day_imputed_activity, aes(steps_per_day)) +
  geom_histogram() +
  labs(title = "Distribution of Steps per Day (Filled NAs)") +
  xlab("Steps per day") +
  ylab('Number of day') +
  theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
The **mean** and **median** of steps taken per day are 1.0766189\times 10^{4} and 1.0766189\times 10^{4}, respectively.

```r
summary(day_imputed_activity$steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. **The used data is not-filled NAs**

```r
week_activity <- activity %>% 
  mutate(week = ifelse(weekdays(as.Date(date)) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday'))
head(week_activity)
```

```
##   steps       date interval    week
## 1    NA 2012-10-01        0 weekday
## 2    NA 2012-10-01        5 weekday
## 3    NA 2012-10-01       10 weekday
## 4    NA 2012-10-01       15 weekday
## 5    NA 2012-10-01       20 weekday
## 6    NA 2012-10-01       25 weekday
```
2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
Make variable for internal steps.

```r
week_interval_activity <- week_activity %>% 
  group_by(interval, week) %>% 
  summarise(steps_per_interval = mean(steps, na.rm = TRUE)) 
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
head(week_interval_activity)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval week    steps_per_interval
##      <int> <chr>                <dbl>
## 1        0 weekday              2.33 
## 2        0 weekend              0    
## 3        5 weekday              0.462
## 4        5 weekend              0    
## 5       10 weekday              0.179
## 6       10 weekend              0
```


```r
ggplot(week_interval_activity, aes(x = interval, y = steps_per_interval)) +
  geom_line() +
  facet_grid(rows = vars(week)) +
  labs(title = "Average number of steps taken across all days by weekdays") +
  xlab("5-minute interval") +
  ylab('Average number of steps') +
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

knit the file

```r
knit2html('PA1_template.Rmd','PA1_template.html')
```

