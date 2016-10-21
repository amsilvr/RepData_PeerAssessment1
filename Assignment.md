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
        hist(steps.tot)
```

![](Assignment_files/figure-markdown_github/histogram_total-1.png)

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

![](Assignment_files/figure-markdown_github/mean_daily-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
        subset(mean_daily, mean_daily == max(mean_daily))
```

    ##      835 
    ## 206.1698

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

        sum(is.na(AMD))

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean for that interval.

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

2.  Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2.  Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png)

**Your plot will look different from the one above** because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.
