library(statsr)
library(dplyr)
library(ggplot2)

# LOAD DATA INTO ENVIRONMENT
data("nycflights")
# CALL STRUCTURE
str(nycflights)
# PLOT DEPARTURE DELAY
ggplot(data = nycflights, aes(x = dep_delay)) + geom_histogram(binwidth = 50)

# FLIGHTS TO RDU
rdu <- nycflights %>%
        filter(dest == 'RDU')
# SUMMARIZE DEPARTURE DELAY STATS LEAVING RDU
rdu %>%
        summarise(mean_delay = mean(dep_delay), sd_delay = sd(dep_delay), n = n())

# FLIGHTS TO SFO IN FEBRUARY
sfo_flights <- nycflights %>%
        filter(dest == 'SFO', month == 2)

# PLOT SFO ARRIVAL DELAYS
ggplot(data = sfo_flights, aes(x = arr_delay)) + geom_histogram(binwidth = 35)

# ANALYZE ORIGIN DIFFERENCES IN ARRIVAL DELAY
sfo_flights %>%
        group_by(origin) %>%
        summarise(mean_arrival = mean(arr_delay), sd_arrival = sd(arr_delay), n = n())

# ANALYZE DELAYS BY CARRIER
sfo_flights %>%
        group_by(carrier) %>%
        summarise(median_delay = median(arr_delay), iqr_delay = IQR(arr_delay), n = n())

# ANALYZE DELAYS BY MONTH + SORT HIGH TO LOW
nycflights %>%
        group_by(month) %>%
        summarise(meanArrivalDelay = mean(arr_delay)) %>%
        arrange(desc(meanArrivalDelay))

# VISUALIZE DELAYS PER MONTH
ggplot(data = nycflights, aes(x = factor(month), y = dep_delay)) + geom_boxplot()

# FIND ON TIME FLIGHTS
nycflights <- nycflights %>%
        mutate(dep_type = ifelse(test = dep_delay < 5,
                                yes = 'on time',
                                no = 'delayed'))

# WHICH AIRPORT GENERATES MOST ON TIME FLIGHTS?
nycflights %>%
        group_by(origin) %>%
        summarise(onTimeRate = sum(dep_type == 'on time') / n()) %>%
        arrange(desc(onTimeRate))

# VISUALIZE DEPARTURE RATE
ggplot(data = nycflights, aes(x = origin, fill = dep_type)) + geom_bar()

# CALCULATE AVERAGE AIR SPEED
nycflights <- nycflights %>%
        mutate(averageAirSpeed = distance / (air_time / 60))

# FASTEST MODEL PLANE
nycflights %>%
        group_by(tailnum) %>%
        summarise(averageSpeed = mean(averageAirSpeed)) %>%
        arrange(averageSpeed)
