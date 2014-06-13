# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
Here read all of the entries of time interval and put them into different groups according to their dates.

```r
data <- read.csv("activity.csv", colClasses = "character")
nData <- nrow(data)
dates <- unique(data$date)
nDates <- length(dates)
nSteps <- NULL

for (i in 1:nDates){
    nSteps[i] <- 0
    for (j in 1:nData){
        if (data$date[j] == dates[i] & !is.na(data$steps[j])){
            nSteps[i] <- nSteps[i] + as.numeric(data$steps[j])
        }
    }
}
```
## What is mean total number of steps taken per day?
The histogram of the total number of steps taken each day is shown below.

```r
plot(1:61, nSteps, "h", xaxt="n", xlab="", ylab="")
axis(1, at=1:61, label=dates)
title(main="Histogram of the Total Number of Steps Taken Each Day", ylab="Number of Steps", xlab="Day") 
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and median total number of steps taken per day are shown below. The mean value is 9354 and the median value is 10395.

```r
meanStep <- mean(nSteps)
print(meanStep)
```

```
## [1] 9354
```

```r
medianStep <- median(nSteps)
print(medianStep)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
Calculate the average steps in each time interval across all days.

```r
averageSteps <- NULL
nValid <- NULL

timeLine <- c(1:12, 21:32, 41:52, 61:72, 81:92, 101:112, 121:132, 141:152, 161:172, 181:192, 201:212, 221:232, 241:252, 261:272, 281:292, 301:312, 321:332, 341:352, 361:372, 381:392, 401:412, 421:432, 441:452, 461:472)
for (i in timeLine){
    averageSteps[i] <- 0
    nValid[i] <- 0
}


for (i in 1:nData){
    if (!is.na(data$step[i])){
        averageSteps[as.numeric(data$interval[i])/5 + 1] <- averageSteps[as.numeric(data$interval[i])/5 + 1] + as.numeric(data$steps[i])
        nValid[as.numeric(data$interval[i])/5 + 1] <- nValid[as.numeric(data$interval[i])/5 + 1] + 1
    }
}

for (i in timeLine){
    if (nValid[i] != 0){
        averageSteps[i] <- averageSteps[i]/nValid[i]
    }
}

plot(1:288, averageSteps[timeLine], type="l", xaxt="n", xlab="", ylab="")
axis(1, at=1:288, label=5*(timeLine-1))
title(main="Average Daily Activity Pattern", ylab="Steps", xlab="Time Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Find which 5-minute interval contains the maximum number of steps.

```r
maxNum <- 0
maxId <- 0
for (i in timeLine){
    if (averageSteps[i] > maxNum){
        maxNum <- averageSteps[i]
        maxId <- i
    }
}
print(maxId)
```

```
## [1] 168
```
The 5-minute interval which contains the maximum number of steps is the 168th interval(i.e. interval 835~840).

## Imputing missing values
Calculate and report the total number of missing values in the dataset.

```r
nMissing <- 0
for (i in 1:nData){
    if (is.na(data$steps[i])){
        nMissing <- nMissing + 1
    }
}
print(nMissing)
```

```
## [1] 2304
```
The number of missing values is 2304.

As for the strategy for filling in all of the missing values in the dataset, we use the mean for that 5-minute interval. The new dataset is named "newData".

```r
newData <- data

for (i in 1:nData){
    if (is.na(data$steps[i])){
        newData$steps[i] <- averageSteps[as.numeric(data$interval[i])/5 + 1]
    }
}

#Histogram of the new data
newNSteps <- NULL
for (i in 1:nDates){
    newNSteps[i] <- 0
    for (j in 1:nData){
        if (newData$date[j] == dates[i]){
            newNSteps[i] <- newNSteps[i] + as.numeric(newData$steps[j])
        }
    }
}

plot(1:61, newNSteps, "h", xaxt="n", xlab="", ylab="")
axis(1, at=1:61, label=dates)
title(main="Histogram of the Total Number of Steps Taken Each Day in Filled-in Data", ylab="Number of Steps", xlab="Day") 
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The mean and median total number of steps taken are the same, 10766.189.

```r
newMeanStep <- mean(newNSteps)
print(newMeanStep)
```

```
## [1] 10766
```

```r
newMedianStep <- median(newNSteps)
print(newMedianStep)
```

```
## [1] 10766
```
The values of mean and median total number of steps after imputing missing values are different from that of the former dataset. Both values increase because we assign the mean value of the 5-minute interval which the missing values are in to the missing values.

## Are there differences in activity patterns between weekdays and weekends?
Add day to newData.

```r
for (i in 1:nData){
    if (weekdays(as.Date(newData$date[i])) == "Saturday" || weekdays(as.Date(newData$date[i])) == "Sunday"){
        newData$day[i] <- "Weekend"
    }
    else{
        newData$day[i] <- "Weekday"
    }
}
```

Calculate the avearge values of each 5-minute interval for weekday and weekend.

```r
averageSteps_1 <- NULL
nValid_1 <- NULL
averageSteps_2 <- NULL
nValid_2 <- NULL
for (i in timeLine){
    averageSteps_1[i] <- 0
    nValid_1[i] <- 0
    averageSteps_2[i] <- 0
    nValid_2[i] <- 0
}


for (i in 1:nData){
    if (newData$day[i] == "Weekday"){
        averageSteps_1[as.numeric(newData$interval[i])/5 + 1] <- averageSteps_1[as.numeric(newData$interval[i])/5 + 1] + as.numeric(newData$steps[i])
        nValid_1[as.numeric(newData$interval[i])/5 + 1] <- nValid_1[as.numeric(newData$interval[i])/5 + 1] + 1
    }
    else{
        averageSteps_2[as.numeric(newData$interval[i])/5 + 1] <- averageSteps_2[as.numeric(newData$interval[i])/5 + 1] + as.numeric(newData$steps[i])
        nValid_2[as.numeric(newData$interval[i])/5 + 1] <- nValid_2[as.numeric(newData$interval[i])/5 + 1] + 1
    }
}

for (i in timeLine){
    if (nValid_1[i] != 0){
        averageSteps_1[i] <- averageSteps_1[i]/nValid_1[i]
    }
    if (nValid_2[i] != 0){
        averageSteps_2[i] <- averageSteps_2[i]/nValid_2[i]
    }
}


par(mfrow=c(2,1))

plot(1:288, averageSteps_1[timeLine], type="l", xaxt="n", xlab="", ylab="")
axis(1, at=1:288, label=5*(timeLine-1))
title(main="Average Daily Activity Pattern of Weekdays", ylab="Steps", xlab="Time Interval")


plot(1:288, averageSteps_2[timeLine], type="l", xaxt="n", xlab="", ylab="")
axis(1, at=1:288, label=5*(timeLine-1))
title(main="Average Daily Activity Pattern of Weekends", ylab="Steps", xlab="Time Interval")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

The panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days and weekend days is shown above.



