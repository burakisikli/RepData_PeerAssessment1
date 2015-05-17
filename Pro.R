activity <- read.csv("activity.csv")

agg_data <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
barplot(agg_data$steps, names.arg = agg_data$date, xlab = "Day", ylab = "Steps")
hist(agg_data$steps, main="Total steps by day", xlab="Day", col="red")

mean(agg_data$steps)
median(agg_data$steps)

agg_data <- aggregate(steps ~ interval, data=activity, mean)
plot(agg_data, type="l",xlab = "5-min interval", ylab = "Average across all days (Steps)", 
     main = "Average number of steps taken")

agg_data$interval[which.max(agg_data$steps)]
sum(is.na(activity))

agg_data <- aggregate(steps ~ interval, data=activity, median)
newActivity <- merge(activity, agg_data, by="interval", suffixes=c("",".median"))
missingAct <- is.na(newActivity$steps)
newActivity$steps[missingAct] <- newActivity$steps.median[missingAct]
newActivity <- newActivity[,c(1:3)]

agg_data <- aggregate(steps ~ date, data=newActivity, sum)
barplot(agg_data$steps, names.arg = agg_data$date, xlab = "Day", ylab = "Steps")
hist(agg_data$steps, main="Total steps by day", xlab="Day", col="red")
mean(agg_data$steps)
median(agg_data$steps)


dayFlag <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$dayFlag <- as.factor(sapply(activity$date, dayFlag))

library(lattice)
agg_data <- aggregate(steps ~ interval + dayFlag, data=activity, mean)
names(agg_data) <- c("interval", "dayFlag", "steps")
xyplot(steps ~ interval | dayFlag, agg_data, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
