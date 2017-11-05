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



SaveFile <- '~/lab/Rpkgs/ConcurrentActivity/data/test-large1.rds'

SaveFile <- '~/lab/Rpkgs/ConcurrentActivity/data/test-large2.rds'

 7,954,626
22,954,489


log_data <- NULL
system.time(
    log_data  <- readRDS(SaveFile)
)
log_data$StartTime <- log_data$Time - log_data$Duration # Fast   0.071   0.048   0.119

temp1 <- log_data


## Remove outlier time data

mdate   <- median(median(as.Date(temp1$Time))) # Chop off data at XXX UTC since we are using only daily file sets most of the time
mintime <- as.POSIXct(paste(as.character(mdate), '00:00:00'), tz="UTC", origin="1970-01-01")
maxtime <- as.POSIXct(paste(as.character(mdate), '22:45:00'), tz="UTC", origin="1970-01-01")

temp1  <-  temp1 %>% filter(Time >= mintime & Time <= maxtime)
temp1  <-  temp1 %>% filter(StartTime >= mintime & StartTime <= maxtime) # Could have a long duration, so check computed start, or limit duration above

MinTime <- min(temp1$StartTime)
MaxTime <- max(temp1$StartTime)

timerange_s       <- as.integer(difftime(max(temp1$StartTime), min(temp1$StartTime), units = 'sec')) + 1 # Number of second bins
temp1$StartSecond <- as.integer(difftime(temp1$StartTime, MinTime, units = 'sec'))                       # Index, 0 is first channel for C++

str(temp1)
nrow(temp1)
timerange_s
MinTime
MaxTime


system.time(
    cca2 <- concurrentActivity(timerange_s, temp1$StartSecond, temp1$Duration)
)

str(cca2)

cca_df      <- data.frame(ConcurrentSessions = cca2)
cca_df$Time <- MinTime + seconds(seq(1:nrow(cca_df)) - 1)

mdate   <- median(median(as.Date(cca_df$Time))) # Chop off data at XXX UTC since we are using only daily file sets most of the time
mintime <- as.POSIXct(paste(as.character(mdate), '00:00:00'), tz="UTC", origin="1970-01-01")
maxtime <- as.POSIXct(paste(as.character(mdate), '22:45:00'), tz="UTC", origin="1970-01-01")
cca_df  <- cca_df %>% filter(Time >= mintime & Time <= maxtime)

PointSize <- 0.6

ggplot(cca_df) +
    geom_line(aes(x = Time,  y = ConcurrentSessions), size=0.05) +
    geom_point(aes(x = Time, y = ConcurrentSessions), size=PointSize, color = 'blue', shape=19) +
    xlab("") +
    scale_x_datetime(date_minor_breaks = "1 hour") +
    ggtitle('Concurrent Sessions Computed from Log')

