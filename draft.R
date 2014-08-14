# Reproducible Research: Peer Assessment 1
============================================
## Loading and preprocessing the data

setwd("~/RepData_PeerAssessment1")
unzip("~/RepData_PeerAssessment1/activity.zip")
data <- read.csv("./activity.csv")
data$date <- as.POSIXct(data$date)
data$interval <- as.factor(data$interval)

## What is mean total number of steps taken per day?
StepsPerDay <- tapply(data$steps, data$date, sum)

#1. Make a histogram of the total number of steps taken each day
hist(StepsPerDay, breaks = 20, main = "Total Number of Steps Taken Each Day")

#2. Calculate and report the mean and median total number of steps taken per day
stepsmean <- mean(StepsPerDay, na.rm = TRUE)
stepsmedian <- median (StepsPerDay, na.rm = TRUE)

## What is the average daily activity pattern?
meanBYinterval <- as.numeric(tapply(data$steps, data$interval, 
                                    FUN = mean, na.rm = TRUE, simplify = FALSE))
plot(unique(data$interval), meanBYinterval, type = "l", 
     main = "Average Number of Steps Taken", xlab = "Interval", 
     ylab = "Average Number of Steps")
meanBYinterval <- as.data.frame(meanBYinterval)
meanBYinterval$interval <- unique(data$interval)
maxmean <- meanBYinterval[which.max(meanBYinterval$meanBYinterval), 2] 
maxmean

## Imputing missing values
## Are there differences in activity patterns between weekdays and weekends?