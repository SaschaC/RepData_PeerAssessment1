library(dplyr)
library(ggplot2)

setwd("~/RepData_PeerAssessment1")

if (!file.exists('./activity.csv')) {
  unzip('activity.zip', unzip = "unzip")
}

dataset_original <- tbl_df(read.csv('activity.csv'))
dataset_original <- transform(dataset_original, date = as.Date(date, "%Y-%m-%d"))

# Calculate the total number of steps taken per day

total_per_day <- group_by(dataset_original, date)%>%summarize(total_steps = sum(steps)); total_per_day


# Make a histogram of the total number of steps taken each day

hist(total_per_day$total_steps)

# Calculate and report the mean and median of the total number of steps taken per day

mean(total_per_day$total_steps, na.rm = T)
median(total_per_day$total_steps, na.rm = T)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the #average number of steps taken, averaged across all days (y-axis)

average_per_interval <- group_by(dataset_original, interval)%>%summarize(average_steps = mean(steps, na.rm = T))
plot(average_per_interval$average_steps ~ set_2$interval, type = "l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps???

filter(average_per_interval, average_steps == max(average_steps))

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sapply(dataset_original, function(x) length(which(is.na(x))))

#Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.

dataset_2 <- dataset_original[,] 

for (index in seq_along(dataset_2$steps)) {

  if (is.na(dataset_2$steps[index])) {
    dataset_2$steps[index] <- mean(dataset_2$steps[dataset_2$interval==dataset_2$interval[index]], na.rm = T)
  }
}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

total_per_day <- group_by(dataset_2, date)%>%summarize(total_steps = sum(steps))

hist(total_per_day$total_steps)

mean(total_per_day$total_steps)
median(total_per_day$total_steps)

# Are there differences in activity patterns between weekdays and weekends?

dataset_2$part_of_week[weekdays(dataset_2$date)%in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] = 'weekday'
dataset_2$part_of_week[weekdays(dataset_2$date)%in% c("Saturday", "Sunday")] = 'weekend'

average_per_interval_poweek <- group_by(dataset_2, interval, part_of_week)%>%summarize(average_steps = mean(steps))

g <- ggplot(average_per_interval_poweek, aes(interval, average_steps))
g + geom_line() + facet_grid(part_of_week~.)
