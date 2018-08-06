# Analyzes journey durations in traffic in Bruxelles, Belgium
# (c) Jean-Etienne Poirrier, 2018 - under GNU GPL v3
# Blog post: https://jepoirrier.org/2018/08/06/time-commuting-in-belgium/
# Dataset: https://github.com/jepoirrier/datasets/blob/master/traffic-20152016.csv

library(lubridate)
library(ggplot2)
library(reshape)

### 0. fist some cleaning and initial variables
rm(list = ls())
mydir = "~/perso/traffic" #platform dependent, to change
myfile = "traffic-20152016.csv"

### 1. opening the file and cleaning it up

if(dir.exists(mydir)) setwd(mydir) else stop()
mydat = read.csv(myfile, header = TRUE, sep = " ", na.strings = c("NA", "na"), colClasses="character")

# creating a proper date
mydat$date = parse_date_time(mydat$YYMMDD, "%y%m%d")

# changing time in base 10
mydat$Astart10 <- as.integer(substr(mydat$Astart, 1, 2)) + (as.integer(substr(mydat$Astart, 3, 4))/60)
mydat$Astop10 <- as.integer(substr(mydat$Astop, 1, 2)) + (as.integer(substr(mydat$Astop, 3, 4))/60)
mydat$Rstart10 <- as.integer(substr(mydat$Rstart, 1, 2)) + (as.integer(substr(mydat$Rstart, 3, 4))/60)
mydat$Rstop10 <- as.integer(substr(mydat$Rstop, 1, 2)) + (as.integer(substr(mydat$Rstop, 3, 4))/60)

# duplicate data set for later
tmp <- mydat

# checking times are increasing each day (or stop)
if(!all(mydat$Astart10 < mydat$Astop10, na.rm=TRUE)) {
  message("At least one value in Astart is > Astop. Check your data.")
  stop()
}
if(!all(mydat$Astop10 < mydat$Rstart10, na.rm=TRUE)) {
  message("At least one value in Astop is > Rstart Check your data.")
  stop()
}
if(!all(mydat$Rstart10 < mydat$Rstop10, na.rm=TRUE)) {
  message("At least one value in Rstart is > Rstop Check your data.")
  stop()
}

# converting 4 journeys into 4 standard dates
mydat$T1 = parse_date_time(paste(mydat$YYMMDD, mydat$Astart, sep = " "),"%y%m%d %H%M")
mydat$T2 = parse_date_time(paste(mydat$YYMMDD, mydat$Astop, sep = " "),"%y%m%d %H%M")
mydat$T3 = parse_date_time(paste(mydat$YYMMDD, mydat$Rstart, sep = " "),"%y%m%d %H%M")
mydat$T4 = parse_date_time(paste(mydat$YYMMDD, mydat$Rstop, sep = " "),"%y%m%d %H%M")

### 2. Analysis starts here

# earliest - latest times - TODO averages
homeDepartureEarliest = min(mydat$Astart, na.rm = TRUE)
homeDepartureLatest = max(mydat$Astart, na.rm = TRUE)
homeArrivalEarliest = min(mydat$Rstop, na.rm = TRUE)
homeArrivalLatest = max(mydat$Rstop, na.rm = TRUE)
workArrivalEarliest = min(mydat$Astop, na.rm = TRUE)
workArrivalLatest = max(mydat$Astop, na.rm = TRUE)
workDepartureEarliest = min(mydat$Rstart, na.rm = TRUE)
workDepartureLatest = max(mydat$Rstart, na.rm = TRUE)

message(paste("Depature from home, min:", homeDepartureEarliest, "; max:", homeDepartureLatest, sep = " "))
message(paste("Arrival at work, min:", workArrivalEarliest, "; max:", workArrivalLatest, sep = " "))
message(paste("Departure from work, min:", workDepartureEarliest, "; max:", workDepartureLatest, sep = " "))
message(paste("Arrival at home, min:", homeArrivalEarliest, "; max:", homeArrivalLatest, sep = " "))

# longest - shortest journeys - TODO averages
mydat$J1 = interval(mydat$T1, mydat$T2) %/% minutes(1)
mydat$J2 = interval(mydat$T3, mydat$T4) %/% minutes(1)
mydat$W = interval(mydat$T2, mydat$T3) %/% minutes(1) # keep it for longest time in office
mydat$Wh = interval(mydat$T2, mydat$T3) %/% hours(1)
mydat$Wm = interval(mydat$T2, mydat$T3) %% hours(1) %/% minutes(1)

toWorkShortest = min(mydat$J1, na.rm = TRUE)
toWorkLongest = max(mydat$J1, na.rm = TRUE)
toHomeShortest = min(mydat$J2, na.rm = TRUE)
toHomeLongest = max(mydat$J2, na.rm = TRUE)
atWorkShortest = min(mydat$W, na.rm = TRUE)
atWorkLongest = max(mydat$W, na.rm = TRUE)

message(paste("Trip to work, min:", toWorkShortest, "minutes; max:", toWorkLongest, "minutes", sep = " "))
message(paste("Trip to home, min:", toHomeShortest, "minutes; max:", toHomeLongest, "minutes", sep = " "))
message(paste("Time in office, min:", atWorkShortest, "minutes; max:", atWorkLongest, "minutes", sep = " "))

### 3. Graphs

# All points

tmp <- subset(mydat, select=c(date, Astart, Astop, Rstart, Rstop))
tmp <- melt(tmp, id = c("date"), measured = c("Astart", "Astop", "Rstart", "Rstop"))
tmp$h10 <- as.integer(substr(tmp$value, 1, 2)) + (as.integer(substr(tmp$value, 3, 4))/60)

ggplot(tmp, aes(x = date, y = h10,
                colour = factor(variable,
                                labels = c("Departure start", "Departure stop", "Return start", "Return stop")))) +
  geom_point() +
  xlab("Date") + ylab("Time") + ggtitle("All traffic points") +
  ylim(0,23) +
  labs(color = "Time at")

# duration of departure journey in function of departure start time

tmp <- subset(mydat, select = c(Astart, Astop, Rstart, Rstop, Astart10, Astop10, Rstart10, Rstop10))
tmp$departureDuration <- (tmp$Astop10 - tmp$Astart10) *60
tmp$returnDuration <- (tmp$Rstop10 - tmp$Rstart10) *60

tmp2 <- subset(tmp, select = c(Rstart10, returnDuration))
tmp2 <- melt(tmp2, id = c("Rstart10"), measured = c("returnDuration"))
names(tmp2)[names(tmp2) == "Rstart10"] <- "startTime"
tmp <- subset(tmp, select = c(Astart10, departureDuration))
tmp <- melt(tmp, id = c("Astart10"), measured = c("departureDuration"))
names(tmp)[names(tmp) == "Astart10"] <- "startTime"
tmp <- rbind(tmp, tmp2)

ggplot(tmp, aes(x = startTime, y = value,
                colour = factor(variable,
                                labels = c("To work", "Home")))) +
  geom_point() +
  xlab("Start Time") + ylab("Journey duration (minutes)") + ggtitle("Time spent in the car") +
  labs(color = "Going:") +
  geom_smooth(method = "lm",formula = y ~ x)

# time spent at work

tmp <- subset(mydat, select = c(date, Astop10, Rstart10))
tmp$durationAtWork <- tmp$Rstart10 - tmp$Astop10
tmp <- subset(tmp, select = c(date, durationAtWork))

tmp <- melt(tmp, id = c("date"), measured = c("durationAtWork"))

summary(tmp$value)

ggplot(tmp, aes(x = date, y = value)) +
  geom_point() +
  xlab("Date") + ylab("Time spent at work (hours)") + ggtitle("Time at work") +
  geom_smooth(method = "lm",formula = y ~ x)

