Reproducible Data Assignment 1
================

Data
----

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) \[52K\]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD format

-   **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading and preprocessing the data

Show any code that is needed to

1.  Load the data (i.e. `read.csv()`)

``` r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- "data/temp.zip"

if (!file.exists(temp)) {
        download.file(url = fileUrl, destfile = temp)
        date_Downloaded <- date()
}

dfNames <- unzip(temp, list = FALSE, exdir = "data")

if (!exists("AMD")) {
        AMD <- as_tibble(read.csv(dfNames[1]))
}
```

1.  Process/transform the data (if necessary) into a format suitable for your analysis

``` r
        AMD$date <- as.Date.factor(AMD$date)
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.  Make a histogram of the total number of steps taken each day

``` r
        steps.tot <- with(AMD, tapply(steps, INDEX = date, FUN = sum))
        hist(steps.tot,
            col = "red", 
            main = "Mean Steps", 
            xlab = "Five-Minute Interval", 
            ylab = "Number of Steps"
            )
```

![](README_files/figure-markdown_github/histogram_total-1.png)

1.  Calculate and report the **mean** and **median** total number of steps taken per day

``` r
mean(steps.tot, na.rm = TRUE)
```

    ## [1] 10766.19

``` r
median(steps.tot, na.rm = TRUE)
```

    ## [1] 10765

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
        mean_daily <- with(AMD, tapply(steps, INDEX = interval, FUN = mean, na.rm = TRUE))

        plot(names(mean_daily),mean_daily, type = "l")
```

![](README_files/figure-markdown_github/mean_daily-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
        subset(mean_daily, mean_daily == max(mean_daily))
```

    ##      835 
    ## 206.1698

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

``` r
sum(is.na(AMD))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

> I will use the mean for that interval.

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
# look at the values of AMD for interval and steps where AMD is.na
# if AMD[i, steps] == NA, AMD1$steps <- mean_daily$steps for given interval  
AMD1 <- AMD
AMD1$interval <- as.character(AMD1$interval)
        for (i in seq_along(AMD1$steps)) {
                t <- AMD1$interval[i]
                   if (is.na(AMD1$steps[i])) {
                       #get the mean steps for the given interval
                        AMD1$steps[i] <- mean_daily[t]
                }
                else (AMD1$steps[i] <- AMD1$steps[i])
        }
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
 steps.tot.imp <- with(AMD1, tapply(steps, INDEX = date, FUN = sum))
        par(mfrow = c(1,2))
        hist(steps.tot, 
            col = "red", 
            main = "Mean Steps with Missing Values", 
            xlab = "Five-Minute Interval", 
            ylab = "Number of Steps"
            )
        hist(steps.tot.imp,
            col = "red", 
            main = "Mean Steps with Imputed Data", 
            xlab = "Five-Minute Interval", 
            ylab = "Number of Steps"
            )
```

![](README_files/figure-markdown_github/imputed_calculations-1.png)

``` r
imputed.calcs <- data.frame(
        "mean (missing)" = mean(steps.tot, na.rm = TRUE),
        " | median (missing)" = median(steps.tot, na.rm = TRUE),
        " | mean (imputed)" = mean(steps.tot.imp),
        " | median (imputed)" = median(steps.tot.imp), check.names = FALSE
)

print(imputed.calcs)
```

    ##   mean (missing)  | median (missing)  | mean (imputed)  | median (imputed)
    ## 1       10766.19               10765          10766.19            10766.19

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
#character vectors for weekday and weekend
weekday <- c("Mon", "Tue", "Wed", "Thu", "Fri")
weekend <- c("Sat", "Sun")

#create wk() function to return weekday or weekend
#
wk <- function(date) {
        if (date %in% weekday) {
                return("weekday")
        }
        if (date %in% weekend) {
                return("weekend")
        }
}

#add the wd column which temporarily holds the actual weekday
AMD.day <- mutate(AMD1, wd = (weekdays(date, abbreviate = TRUE)), interval = as.numeric(interval))

#loop through and perform wk() on each observation in AMD.day
# question - could I do this all in the step above?

for (i in seq_along(AMD.day$wd)) {
        AMD.day$wd[i] <- wk(AMD.day$wd[i])
}
```

1.  Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
# use dplyr functions to group and summarize by day type and interval
by_daytype <- group_by(AMD.day, wd = as.factor(wd), interval) %>%
              summarize(steps = mean(steps))

#create ggplot object g
g <- ggplot(data = by_daytype, mapping = aes(interval, steps))

#plot
g + geom_line() + facet_wrap(~wd, nrow = 2)
```

![](README_files/figure-markdown_github/end_vs_day-1.png)
