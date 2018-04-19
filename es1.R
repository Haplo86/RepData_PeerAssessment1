# Code for reading in the dataset and/or processing the data

library(dplyr)
library(ggplot2)

act <- read.table('activity.csv', sep = ',', header = T)

tail(act)
str(act)
tbl_df(act)

# Histogram of the total number of steps taken each day

sum  <- act %>% group_by(date) %>% summarise(steps_sum = sum(steps, na.rm = T))

p <- ggplot(sum, aes(x = date, y = steps_sum))
p + geom_histogram(stat = 'identity', fill = 'firebrick') +
        labs(title = 'Steps taken per day without missing values', x = 'Date', y = 'Total steps') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

# Mean and median number of steps taken each day

mean(sum$steps_sum)
median(sum$steps_sum)

# act2 <- ACT %>% mutate(steps2 = steps) %>% group_by(date) %>% 
#         summarise(steps_mean = mean(steps, na.rm = TRUE),
#                   steps_median = median(steps2, na.rm = TRUE))

# Time series plot of the average number of steps taken

average <- act %>% group_by(interval) %>% summarise(steps_mean = mean(steps, na.rm = T))

p2 <- ggplot(average, aes(x = interval, y = steps_mean, group = 1))
p2 + geom_line(col = 'firebrick') + labs(title = 'Steps taken on average per interval', 
                                         x = 'Intervals',
                                         y = 'Average steps per interval')

# The 5-minute interval that, on average, contains the maximum number of steps

max_avg <- which.max(average$steps_mean)
average[max_avg,1]

# Code to describe and show a strategy for imputing missing data

length(act[is.na(act[,1]),1])

incomplete_values <- act[is.na(act[,1]),]
average <- as.data.frame(average)
merged_values <- merge(incomplete_values, average, by = 'interval', all.x = T)

sorted_values <- merged_values[order(merged_values$date, merged_values$interval),]
act2 <- act
act2[is.na(act2[,1]),1] <- sorted_values[,4]

# Histogram of the total number of steps taken each day after missing values are imputed

sum2  <- act2 %>% group_by(date) %>% summarise(steps_sum = sum(steps, na.rm = T))

p3 <- ggplot(sum2, aes(x = date, y = steps_sum))
p3 + geom_histogram(stat = 'identity', fill = 'firebrick') +
        labs(title = 'Steps taken per day with missing values', x = 'Date',
             y = 'Total steps') + theme(axis.text.x = element_text(angle = 90, 
                                                                   vjust = 0.4))

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Sys.setlocale("LC_ALL","English")

act2[,2] <- as.POSIXct(strptime(as.character(act2[,2]), '%Y-%m-%d'))
act2[,2] <- format(act2[,2], format = '%Y-%m-%a')

act2[,4] <- 'weekdays'
act2[grepl('Sat', act2[,2]) | grepl('Sun', act2[,2]),4] <- 'weekends'
act2[,4] <- as.factor(act2[,4])
colnames(act2) <- c('steps', 'date', 'interval', 'days')

act2_avg <- act2 %>% group_by(interval, days) %>%
        summarise(steps_mean = mean(steps, na.rm = T))

p4 <- ggplot(act2_avg, aes(x = interval, y = steps_mean, col = days))
p4 + geom_line() +labs(title = 'Steps taken on average in per interval', 
             x = 'Intervals', y = 'Average steps per Interval',
             col = 'Days of the week') + facet_grid(days~.)


































