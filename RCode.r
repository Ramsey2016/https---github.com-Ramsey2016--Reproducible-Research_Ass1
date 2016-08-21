## ----loaddata------------------------------------------------------------
require(lattice) 
library(data.table)
library(ggplot2)

#First I want to make sure that default directory is set right before I start with the work
setwd("C:\\RSWeek2")
# we define name of the file in unzipped and zipped format and check if unzipped format exists. If not, we unzip it'
file = "activity.csv"
zipfilename = "repdata_data_activity.zip"
if (!file.exists(file)) {
  unzip(zipfilename)
}

dataset <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))

dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")
dataset$interval <- as.factor(dataset$interval)

str(dataset)

# What is mean total number of steps taken per day?

steps_per_day <- aggregate(steps ~ date, dataset, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)

ggplot(steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "orange", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)

#What is the average daily activity pattern?

steps_per_interval <- aggregate(dataset$steps, 
                                by = list(interval = dataset$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers
##this helps in plotting
steps_per_interval$interval <- 
  as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
  geom_line(color="orange", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
  theme_bw()

max_interval <- steps_per_interval[which.max(  
  steps_per_interval$steps),]
#Imputing missing values

missing_vals <- sum(is.na(dataset$steps))

#filling na's

na_fill <- function(data, pervalue) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}

dataset_fill <- data.frame(  
  steps = na_fill(dataset, steps_per_interval),  
  date = dataset$date,  
  interval = dataset$interval)
str(dataset_fill)

sum(is.na(rdata_fill$steps))

# A histogram of the total number of steps taken each day

fill_steps_per_day <- aggregate(steps ~ date, dataset_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
#Calculate and report the mean and median total number of steps taken per day.

# Are there differences in activity patterns between weekdays and weekends?

weekdays_steps <- function(data) {
  weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                              FUN=mean, na.rm=T)
  # convert to integers for plotting
  weekdays_steps$interval <- 
    as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
  colnames(weekdays_steps) <- c("interval", "steps")
  weekdays_steps
}

data_by_weekdays <- function(data) {
  data$weekday <- 
    as.factor(weekdays(data$date)) # weekdays
  weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
  weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
  
  weekend_steps <- weekdays_steps(weekend_data)
  weekday_steps <- weekdays_steps(weekday_data)
  
  weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
  weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
  
  data_by_weekdays <- rbind(weekend_steps, weekday_steps)
  data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
  data_by_weekdays
}

data_weekdays <- data_by_weekdays(dataset_fill)

ggplot(data_weekdays, aes(x=interval, y=steps)) + 
  geom_line(color="violet") + 
  facet_wrap(~ dayofweek, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()