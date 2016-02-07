## Reproducible Research - Course project 1
##
##Loading and preprocessing the data
##
##Importing the dataset
setwd("~/Documents/Coursera/DataScienceSpec/5_Reproducible_Research/CourseProject01/CP01")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./activity_monitoring_data.zip",method="curl")
unzip("activity_monitoring_data.zip")

library(ggplot2)

# Loading dataset
activity <- read.csv("activity.csv")
##
## What is mean total number of steps taken per day?
## Calculating the total number of steps taken per day
library(dplyr)
days <- group_by(activity, date)
total_steps <- as.data.frame(summarize(days, total = sum(steps, na.rm = TRUE)))
## Considering only days with total steps greater than 0
total_steps_final <- as.data.frame(filter(total_steps, total > 0))
## histogram of the total number of steps taken each day
ggplot(total_steps_final,aes(x=date,y=total))+ ggtitle("Total number of steps by day") + ylab("total steps") + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8))
## Mean and median number of steps taken each day
mean(total_steps_final$total)
median(total_steps_final$total)
## answer 10766,19 and 10765 respectively
## What is the average daily activity pattern?
##Time series plot of the average number of steps taken
ts.activity <- activity[complete.cases(activity),] ## removing Nas
ts.activity <- filter(ts.activity, steps > 0) ## removing 0 values
interval <- group_by(ts.activity, interval)
mean_interval<-as.data.frame(summarize(interval, interval_mean = mean(steps, na.rm = TRUE)))
par(mfrow = c(1,1))
plot(mean_interval, cex.axis = 0.6, cex.main = 0.8, cex.sub = 0.7, cex.lab = 0.7, main = "Average daily activity pattern", xlab = "5 minute interval", ylab = "Average number of steps across all days", type = "l")
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_avgsteps <- mean_interval[order(mean_interval$interval_mean, decreasing = TRUE),]
max_avgsteps[1,1]
## answer: 835
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity))
## answer: 2304

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
mean_avgsteps <- mean(mean_interval$interval_mean)
rep_activity <- activity
rep_activity <- as.data.frame(activity$steps)

impute.mean <- function(x) replace(x, is.na(x), mean_avgsteps)
rep_activity_wnas <- impute.mean(rep_activity)

##new dataset that is equal to the original dataset but with the missing data filled in a new column "activity$steps"
activity_new <- cbind(activity, rep_activity_wnas)
names(activity_new)<-c("steps", "date", "interval", "steps_new")

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

days2 <- group_by(activity_new, date)
total_steps2 <- as.data.frame(summarize(days2, total = sum(steps_new, na.rm = TRUE)))
## Considering only days with total steps greater than 0
total_steps_final2 <- as.data.frame(filter(total_steps2, total > 0))
## histogram of the total number of steps taken each day
ggplot(total_steps_final2,aes(x=date,y=total))+ ggtitle("Total number of steps by day") + ylab("total steps") + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=8))
## Mean and median number of steps taken each day
mean(total_steps_final2$total)
median(total_steps_final2$total)
##answer 13431.61 and 11458 steps respectively
##
##Are there differences in activity patterns between weekdays and weekends?  
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

library(timeDate)
data_week <- activity
week <- vector(mode="character", length=17568)
aux <- 0
for (i in 1:17568){
aux <- isWeekday(data_week$date[i])
if (aux == TRUE){ 
  week[i]<-c("weekday")
} else{
    week[i]<-c("weekend")
  }
}

activity_week<-cbind(data_week, as.factor(week))
names(activity_week) <- c("steps","date","interval", "week")

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
library(dplyr)
ts.activity2 <- activity_week[complete.cases(activity_week),] ## removing Nas
ts.activity2 <- filter(ts.activity2, steps > 0) ## removing 0 values
interval2 <- group_by(ts.activity2, interval)
interval2 <- as.data.frame(interval2)

interval2_weekdays <- filter(interval2, week=="weekday")
interval2_weekdays <- group_by(interval2_weekdays, interval)
interval2_weekends <- filter(interval2, week=="weekend")
interval2_weekends <- group_by(interval2_weekends, interval)
mean_interval2_weekdays<-as.data.frame(summarize(interval2_weekdays, mean_steps = mean(steps, na.rm = TRUE)))
mean_interval2_weekends<-as.data.frame(summarize(interval2_weekends, mean_steps = mean(steps, na.rm = TRUE)))
par(mfrow = c(1,2))
plot(mean_interval2_weekends,cex.axis = 0.6, cex.main = 0.8, cex.sub = 0.7, cex.lab = 0.7, main = "Average daily activity pattern - Weekends", xlab = "5 minute interval", ylab = "Average number of steps across all days", type = "l")
plot(mean_interval2_weekdays,cex.axis = 0.6, cex.main = 0.8, cex.sub = 0.7, cex.lab = 0.7, main = "Average daily activity pattern - Weekdays", xlab = "5 minute interval", ylab = "Average number of steps across all days", type = "l")
