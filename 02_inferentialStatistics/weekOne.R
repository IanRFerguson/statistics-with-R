library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)

# PLOT AREA STATS FOR ENTIRE SAMPLE
ggplot(data = ames, aes(x = area)) +
        geom_histogram(binwidth = 250)

# DEFINE SAMPLE STATISTICS (MEAN, MEDIAN, SD, IQR, 1ST / 3RD PPERCENTILE)
ames %>%
        summarise(mu = mean(area), pop_med = median(area), 
                  sigma = sd(area), pop_iqr = IQR(area),
                  pop_min = min(area), pop_max = max(area),
                  pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
                  pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

# DRAW RANDOM SAMPLE OF SIZE 50
samp1 <- ames %>%
        sample_n(size = 50)

# DRAW RANDOM SAMPLE OF SIZE 70
samp2 <- ames %>%
        sample_n(size = 70)

# VIEW MEAN OF AREA FOR FIRST SAMPLE (SIZE 50)
samp1 %>%
        summarise(x_bar = mean(area))

# DRAW 150000 RANDOM SAMPLES OF SIZE 150 WITH REPLACEMENT (I.E., SAME POOL EVERY DRAW)
sample_means150 <- ames %>%
        rep_sample_n(size = 150, reps = 15000, replace = TRUE) %>%
        summarise(x_bar = mean(area))

# PLOT MEAN OF RANDOM SAMPLES
ggplot(data = sample_means150, aes(x = x_bar)) +
        geom_histogram(binwidth = 20)

# DRAW 25 RANDOM SAMPLES OF SIZE 10 WITH REPLACEMENT + SUMMARIZE MEANS
sample_means_small <- ames %>%
        rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
        summarise(x_bar = mean(area))

# PLOT MEANS OF ABOVE SAMPLE
ggplot(data = sample_means50, aes(x = x_bar)) +
        geom_histogram(binwidth = 20)
