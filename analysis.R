activity <- read.csv("activity.csv", header =TRUE)

##need as a date to try stuff out
activity$date <- (as.POSIXct(as.character(activity$date), format = "%Y-%m-%d", tz = ""))

##let's take the 0's out as the number of 0's makes the median 0 and distorts the dateset
new_activity <- subset(activity,!steps %in% c(0))

##histogram for total number of steps
ggplot(activity, aes(x=date, y=steps)) + geom_bar(stat="identity", color = "lightblue")  + ggtitle("Total Number of steps over the time period")

##does the mean/median stuff
ddply(new_activity, c("date"), function(X) data.frame(Mean_steps = mean(X$steps, na.rm=TRUE), median_steps = quantile(X$steps, 0.50, na.rm=TRUE)))

##now, for each day and each interval across each day, average the number of steps


##prefer this to show all intervals and label the max value:
g <- ggplot(SDI, aes(x=interval, y=steps.mean)) 
g <- g + geom_line(colour= "blue") + ggtitle("Average steps per interval") 
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 0)) 
g <- g + scale_x_continuous(limits =c(100, 2300), breaks=pretty_breaks(n=100)) 
g <- g + geom_text(data=SDI[which.max(SDI[,3]), ], label="Max", vjust=0.5, colour = "red") 
g <- g + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
g <- g + ylab("Average number of steps")
g <- g + xlab("Interval")
g



