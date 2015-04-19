---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
```{r}
activity <-read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(activity)

library(lattice)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
#Develop the histogram
```{r}
StepsTotal <-aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(StepsTotal$steps, main = "Total Steps by Day", xlab = "day", col = "red")
```

#Find Mean/Median
```{r}
mean(StepsTotal$steps)
median(StepsTotal$steps)
```

## What is the average daily activity pattern?
#Create time series and steps plot
```{r}
time_series <-tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(row.names(time_series), time_series, type = "l", xlab = "5-min Interval", 
     ylab = "Average Across All Days", main = "Average Number of Steps Taken", 
     col = "red")
```

#Which 5 minute interval has the max number of steps
```{r}
max_interval <-which.max(time_series)
names(max_interval)
```

## Imputing missing values
#Calculate the total number of missing values
```{r}
activity_NA <-sum(is.na(activity))
activity_NA
```

#Devise strategy for filling in missing values
```{r}
StepsAverage <-aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <-numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

#Create new dataset with missing values filled in
```{r}
new_activity <-activity
new_activity$steps <-fillNA
```

#Develop the new histogram with new data
```{r}
StepsTotal2 <-aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

#Find Mean/Median
```{r}
mean(StepsTotal2$steps)
median(StepsTotal2$steps)
```

## Are there differences in activity patterns between weekdays and weekends?
#Create new weekday/weekend factor variable
```{r}
day <-weekdays(activity$date)
daylevel <-vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}

activity$daylevel <-daylevel
activity$daylevel <-factor(activity$daylevel)

stepsByDay <-aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <-c("interval", "daylevel", "steps")
```

#Make the panel plot containing the new factor time series 
```{r}
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of Steps")
```