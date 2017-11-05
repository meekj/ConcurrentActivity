## ---- Small Test Set

library(tidyr)

## Read test data

tf <- '~/lab/Rpkgs/ConcurrentActivity/data/test1.txt'

tlog_data <- read_delim(tf, delim = ' ', col_types = list(Time = col_datetime('%Y-%m-%dT%H:%M:%S')), progress = interactive())
tlog_data$StartTime <- tlog_data$Time - tlog_data$Duration

## tlog_data$N <- as.numeric(rownames(tlog_data))

tlog_data$N <- 1:nrow(tlog_data) # Event number

tlog_data


## Plot it - Method 1

lines <- tlog_data %>% select(-Duration) %>% gather(tType, time, StartTime, Time)
lines <- lines %>% mutate(dt = time - min(time))
lines$time <- as.POSIXct(lines$time, tz="UTC", origin="1970-01-01")

lines

PointSize <- 4.0


ggplot(tlog_data) +
    geom_line(data = lines, aes(x = time, y = N, group = N), size = 0.3) +
    geom_point(aes(x = StartTime, y = N), size=PointSize, color = 'blue', shape=19) +
    geom_point(aes(x = Time, y = N), size=PointSize, color = 'red', shape=19) +
    scale_x_datetime(date_minor_breaks = "1 sec") + ggtitle('POSIXct Time - tlog_data')




## Plot it - Method 2

## Re-read

tf <- '~/lab/Rpkgs/ConcurrentActivity/data/test1.txt'

tlog_data <- read_delim(tf, delim = ' ', col_types = list(Time = col_datetime('%Y-%m-%dT%H:%M:%S')), progress = interactive())
tlog_data$StartTime <- tlog_data$Time - tlog_data$Duration
tlog_data$N <- 1:nrow(tlog_data) # Event number

tlog_data

MinTime <- min(as.numeric(tlog_data$StartTime))

tlog_data$Time <- as.numeric(tlog_data$Time) - MinTime
tlog_data$StartTime <- as.numeric(tlog_data$StartTime) - MinTime

tlog_data

lines <- tlog_data %>% select(-Duration) %>% gather(tType, time, StartTime, Time)

lines

tlog_data$Time <- as.numeric(tlog_data$Time)
tlog_data$StartTime <- as.numeric(tlog_data$StartTime)


tlog_data <- tlog_data %>% mutate(RoundedDuration = round(Duration), RoundedStart = round(StartTime))



## tlog_data %>% transmute(Time = Time - MinTime, StartTime = StartTime - MinTime)


MaxTime <- max(tlog_data$Time)

ggplot(tlog_data) +
    geom_line(data = lines, aes(x = time, y = N, group = N), size = 0.3) +
    geom_point(aes(x = StartTime, y = N), size=PointSize, color = 'blue', shape=19) +
    geom_point(aes(x = Time, y = N), size=PointSize, color = 'red', shape=19) +
    geom_point(aes(x = RoundedStart, y = N), size=PointSize, color = 'green', shape=19) +
    scale_x_continuous(minor_breaks = seq(0, MaxTime, 1))



 ## Test algorithm



tf <- '~/lab/Rpkgs/ConcurrentActivity/data/test1.txt'

tlog_data <- read_delim(tf, delim = ' ', col_types = list(Time = col_datetime('%Y-%m-%dT%H:%M:%S')), progress = interactive())
tlog_data$StartTime <- tlog_data$Time - tlog_data$Duration

temp1 <- tlog_data

MinTime <- min(temp1$StartTime)

timerange_s       <- as.integer(difftime(max(temp1$StartTime), min(temp1$StartTime), units = 'sec')) + 1 # Number of second bins
temp1$StartSecond <- as.integer(difftime(temp1$StartTime, MinTime, units = 'sec'))                       # Index, 0 is first channel for C++, fast, but maybe eliminate later

str(temp1)

 cca1 <- concurrentActivity(timerange_s, temp1$StartSecond, temp1$Duration)

 cca1

## -----------------




## -------- Big Data Test - Development Mode  ------------------------

library(Rcpp)

sourceCpp("~/lab/Rpkgs/ConcurrentActivity/src/concurrent_activity.cpp")



