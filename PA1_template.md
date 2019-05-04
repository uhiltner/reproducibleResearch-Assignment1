---
title: "Reproducible Research: Peer Assessment 1"
author: Ulrike Hiltner
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
# Unzip data, if neccessary
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
# Import dataset
activityData <- readr::read_csv('activity.csv') # in contrast to read.csv(), read_csv() parses the variable types. E.g., 'date' has already been recognized as a date.
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Check the types of variables
head(activityData)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <int> <date>        <int>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is the mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
1. Make a histogram of the total number of steps taken each day


```r
# Caclulate sum of steps per day----
# Assign new variable, then ...
stepsDay <- activityData %>%
  # aggregate per day, then ...
  dplyr::group_by(date) %>%
  # calculate total number of steps, then ...
  dplyr::summarise(sum_stepsDay = sum(steps, na.rm = TRUE)) %>%
  # remove grouping information from data frame.
  dplyr::ungroup()
```


```r
# Plot histogram----
ggplot2::ggplot() +
  # design of figure
  theme_bw() +
  coord_cartesian(ylim = c(0,15)) +
  # axis and title
  labs(x = "sum steps per day", 
       y = "frequency (binwidth 1000)",
       title = "Total number of steps taken each day") +
  # visualize data
  geom_histogram(data = stepsDay,
                 mapping = aes(x = sum_stepsDay), 
                 fill = "#9B0094", color = "#000000" ,
                 binwidth = 1000)
```

![](PA1_template_files/figure-html/chunk5-1.png)<!-- -->

```r
# save output if needed  
if(saveAs){
  ggsave("fig_chunk5.png")
}
```

2. Calculate and report the mean and median total number of steps taken
per day


```r
# Caclulate summary statistics of steps per day----
stepsDay_mean <- mean(stepsDay$sum_stepsDay, na.rm = TRUE)
stepsDay_median <- median(stepsDay$sum_stepsDay, na.rm = TRUE)
```

The mean total number of steps per day are 9354.2295082 and the median total number of steps per day are 10395.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)


```r
# Caclulate mean number of steps per 5 minutes----
# Assign new variable, then ...
steps5min <- activityData %>%
  # aggregate per time interval across all days, then ...
  dplyr::group_by(interval) %>%
  # calculate mean number of steps, then ...
  dplyr::summarise(mean_steps5min = mean(steps, na.rm = TRUE)) %>%
  # remove grouping information from data frame
  dplyr::ungroup()
```


```r
# Plot histogram----
ggplot2::ggplot(data = steps5min) +
  labs(x = "time interval", y = "mean steps / 5min",
       title = "Mean number of steps across all days per 5 minutes") +
  geom_line(mapping = aes(x = interval, y = mean_steps5min), 
            color = "#9B0094", size = 1.5)
```

![](PA1_template_files/figure-html/chunk8-1.png)<!-- -->

```r
# save output if needed  
if(saveAs){
  ggsave("fig_chunk8.png")
}
```

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?


```r
maxSteps <- steps5min[which(steps5min$mean_steps5min ==
                              max(steps5min$mean_steps5min)), ]
```

The maximum number of steps are 206.1698113 at the 5-minutes time interval 835.
  
## Imputing missing values

Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)


```r
# Count number of NA in steps and intervals
countNA_steps <- length(which(is.na(activityData$steps)))
countNA_interval <- length(which(is.na(activityData$interval)))
```

There are 2304 NAs contained in 'steps' and 0 in 'interval'.

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.

- I will use the mean for a 5-minute interval to replace all missing values in the dataset. At the end, I will check if all the NAs have been replaced.


```r
# This function interpolates NAs of a variable with mean of that variable.
# It takes one argument: 
impute_mean <- function(column) {
  
  replace(x = column, # vector
          list = is.na(column), # an index vector
          values = mean(column, na.rm = TRUE)) # replacement values
} # end of function

# Assign new variable, then ...
meanDay <- activityData %>% 
  # Aggregate by time interval across all days, then ...
  group_by(interval) %>% 
  # add column with correctec values, then ...
  mutate(steps_corr = impute_mean(steps)) %>%
  # remove grouping info from dataset
  ungroup()

head(meanDay)
```

```
## # A tibble: 6 x 4
##   steps date       interval steps_corr
##   <int> <date>        <int>      <dbl>
## 1    NA 2012-10-01        0     1.72  
## 2    NA 2012-10-01        5     0.340 
## 3    NA 2012-10-01       10     0.132 
## 4    NA 2012-10-01       15     0.151 
## 5    NA 2012-10-01       20     0.0755
## 6    NA 2012-10-01       25     2.09
```


```r
# Calculate the number of NAs in the new column of the dataset 'meanDay'
noNAs <- sum(is.na(meanDay$steps_corr))
```

Check: Now, there are 0 NAs contained in the column 'steps_corr'.

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.


```r
# Assign new variable, then ...
meanDay_corr <- meanDay %>%
  # Drop old 'steps' column, then ...
  dplyr::select(steps_corr, date, interval) %>%
  # change new column name to old one.
  dplyr::rename(steps = steps_corr)

head(meanDay_corr)
```

```
## # A tibble: 6 x 3
##    steps date       interval
##    <dbl> <date>        <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total
daily number of steps?


```r
# Caclulate sum of steps per day----
# Assign new variable, then ...
stepsDay_corr <- meanDay %>%
  # aggregate per day, then ...
  dplyr::group_by(date) %>%
  # calculate total number of steps for raw and corrected values, then ...
  dplyr::summarise(sum_stepsDay = sum(steps, na.rm = TRUE),
                   sumcorr_stepsDay = sum(steps_corr, na.rm = TRUE)) %>%
  # remove grouping information from data frame.
  dplyr::ungroup() %>%
  # reshape dataset to compare both histograms by plotting with ggplot2
  tidyr::gather(key = type, value = sum_stepsDay, sum_stepsDay:sumcorr_stepsDay,
                factor_key = T, na.rm = F)
```


```r
# Plot histogram of corrected values----
ggplot2::ggplot() +
  # design of figure
  theme_bw() +
  coord_cartesian(ylim = c(0,15)) +
  # axis and title
  labs(x = "sum steps per day", 
       y = "frequency (binwidth 1000)",
       title = "Total number of steps taken each day (corrected values)") +
  # visualize subset of data showing the corrected values only
  geom_histogram(data = subset(stepsDay_corr, type == "sumcorr_stepsDay"),
                 mapping = aes(x = sum_stepsDay), 
                 fill = "#B8E100", color = "#000000" ,
                 binwidth = 1000)
```

![](PA1_template_files/figure-html/chunk15-1.png)<!-- -->

```r
# save output if needed  
if(saveAs){
  ggsave("fig_chunk15.png")
}  
```


```r
summaryStats <- stepsDay_corr %>%
  group_by(type) %>%
  summarise(mean = mean(sum_stepsDay, na.rm = TRUE),
            median = median(sum_stepsDay))
```

The corrected mean equals 1.0766189\times 10^{4} and the corrected median equals 1.0766189\times 10^{4}.

The raw mean equals 9354.2295082 and the corrected median equals 1.0395\times 10^{4}.


```r
# Plot histogram----
ggplot2::ggplot() +
  # design of figure
  theme_bw() +
  # axis and titles
  labs(x = "sum steps per day", 
       y = "frequency (binwidth 1000)",
       title = "Comparison of total number of steps taken each day\nby raw and corrected values") +
  # legend and color style
  scale_fill_manual(name = "values:", 
                    values = c("#9B0094", "#B8E100"),
                    labels= c("raw", "corrected")) +
  scale_color_manual(name = "values:", 
                    values = c("#9B0094", "#B8E100"),
                    labels= c("raw", "corrected")) +
  # visualize data
  geom_histogram(data = stepsDay_corr,
                 mapping = aes(x = sum_stepsDay, fill = type), 
                 color = "#000000",
                 binwidth = 1000, position = "dodge") 
```

![](PA1_template_files/figure-html/chunk17-1.png)<!-- -->

```r
# save output if needed  
if(saveAs){
  ggsave("fig_chunk17.png")
}  
```

From the comparison we can see that the maximum of the corrected data is greater than that of the raw data set. The mean values of the two data sets are identical. The medians of the two datasets are slightly different.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset
with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.


```r
# I am using R outside the US, therefore I need to set weekdays() to English
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
# Take dataset, then ...
test <-  meanDay_corr %>%
  # add column indicating weekday or weekend.
  mutate(wday = weekdays(date),
         dayType = if_else(wday %in% c("Saturday", "Sunday"), 
                           "Weekend", "Weekday")) %>%
  group_by(interval, dayType) %>%
  summarise(mean_steps5min = mean(steps)) %>%
  ungroup()
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).


```r
# Plot histogram----
ggplot2::ggplot() +
  # design of figure
  theme_bw() +
  # axis and titles
  labs(x = "time interval", 
       y = "mean steps / 5min",
       title = "Comparison of mean number of steps per 5 minutes\nby weekday and weekend") +
  # legend and color style
  scale_color_manual(name = "type:", 
                    values = c("#9B0094", "#B8E100")) +
  # visualize data
  geom_line(data = test,
            mapping = aes(x = interval, y = mean_steps5min, color = dayType),
            size = 1.5, show.legend = FALSE) +
  # split into multiple panels
  facet_grid(dayType ~ .)
```

![](PA1_template_files/figure-html/chunk19-1.png)<!-- -->

```r
# save output if needed  
if(saveAs){
  ggsave("fig_chunk19.png")
} 
```

There are differences in activity patterns between weekdays and weekends. Compared to weekdays, people tend to get up later at weekends. At weekends, people tend to be more active, probably because they don't work and have more time for exercise.
