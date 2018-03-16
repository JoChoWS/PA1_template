The following is a brief analysis of activity monitoring data. This is my attempt at Course Project \#1 in the Coursera Reproducible Research on-line course by Johns Hopkins University.

First, the necessary R packages and the activity monitoring data are loaded:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(reshape2)

activity_data <- read.csv("C:/Users/JoCho/Desktop/activity.csv")

str(activity_data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
activity_data$date <- as.Date(activity_data$date)
```

Next, the assigned tasks (numbered 1 - 8) are performed.

1.  Histogram of the total number of steps taken each day:

``` r
totals <- activity_data %>%
          group_by(date) %>%
          summarise(steps = sum(steps, na.rm = TRUE))
 
ggplot(totals, aes(steps)) + geom_histogram(fill = "steelblue", color = "black", bins = 40) +
      labs(title = "Total Steps Taken", y = "days")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  Mean and Median of the total number of steps taken per day:

``` r
head(totals) %>%
   summarise(mean = mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE))
```

    ## # A tibble: 1 x 2
    ##    mean median
    ##   <dbl>  <dbl>
    ## 1  8718  11734

1.  Time series plot of the average number of steps taken:

``` r
totals$average <- totals$steps/length(totals$date)

plot(totals$date, totals$average, main = "Daily Step Count Averages", xlab = "Date", 
     ylab = "Average Number of Steps per Five Minutes")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

1.  The 5-minute interval that, on average, contains the maximum number of steps:

``` r
intervals <- activity_data %>% group_by(interval) %>%
  summarise(mean = mean(steps, na.rm=TRUE))

max_int <- max(intervals$mean)

intervals[intervals$mean == max_int,]
```

    ## # A tibble: 1 x 2
    ##   interval     mean
    ##      <int>    <dbl>
    ## 1      835 206.1698

1.  Code to describe and show a strategy for imputing missing data:

First, how much missing data is there?

``` r
summary(activity_data$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    0.00   37.38   12.00  806.00    2304

``` r
NA_ratio = sum(is.na(activity_data$steps))/length(activity_data$steps)
NA_ratio
```

    ## [1] 0.1311475

13% is a significant figure, but amounts to less than 5 of the 61 days of data. Are these 5+ days of data reflective of certain days when the tracker was not worn, or are the missing values randomly scattered?

``` r
ggplot(activity_data, aes(x=date, y=steps)) + 
  geom_line() +
  scale_x_date(date_breaks = '10 days', date_labels = '%m/%d')
```

    ## Warning: Removed 576 rows containing missing values (geom_path).

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

The above plot shows that the large majority of NA values occurred on specific days. Notably, there is no data for Oct.1 and a small amout for October 2. Thus, it appears likely that the subject did not begin tracking his/her steps until late on Oct. 2. Consequently, those two dates will be omitted from the set. The remaining values will be imputed using the mice package. (Against the assignment protocal, I've set results="hide" here to avoid showing the mice function's lengthy output.)

``` r
library(mice)
```

    ## Loading required package: lattice

``` r
activity_data_NArm <- filter(activity_data, date > "2012-10-02")
steps_only <- data.frame(activity_data_NArm[,1], activity_data_NArm[,3])

temp <- mice(data = steps_only, m = 5, method = "pmm", maxit = 50, seed = 500)

complete_steps <- complete(temp,1)

activity_data_NArm$steps <- complete_steps[,1]
```

1.  Histogram of total number of steps per day once missing values are replaced:

``` r
totals_NArm <- activity_data_NArm %>%
               group_by(date) %>%
               summarise(steps = sum(steps, na.rm = TRUE))
 
ggplot(totals_NArm, aes(steps)) + geom_histogram(fill = "green", color = "black", bins = 40) +
      labs(title = "Total Steps Taken (NA Values Removed)", y = "days")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)

1.  Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:

``` r
totals$weekend <- grepl("S.+",weekdays(totals$date))

weekday <- filter(totals, weekend == FALSE)
weekend <- filter(totals, weekend == TRUE)

weekday$average <- weekday$steps/length(weekday$date)
weekend$average <- weekend$steps/length(weekend$date)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(weekday, plot(date, steps, type = "l", main = "Weekday Steps"))
with(weekend, plot(date, steps, type = "l", main = "Weekend Steps"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

Thank you and good night.
