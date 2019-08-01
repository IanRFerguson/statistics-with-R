library(statsr)
library(dplyr)
library(ggplot2)

data(ames)

# DEFINE SAMPLE PARAMETERS
n <- 60
samp <- sample_n(ames, n)

# OBTAIN SAMPLE STATISTICS FOR HOUSE SIZE
summary(samp$area)

# CALCULATE CRITICAL VALUE FOR 95% CONFIDENCE INTERVAL
z_star95 <- qnorm(0.975) # 100 - 2.5 on either end of tail == 97.5

# CALCULATE CONFIDENCE INTERVAL LOWER + UPPER
samp %>%
        summarise(lower = mean(area) - z_star95 * (sd(area) / sqrt(n)),
                  upper = mean(area) + z_star95 * (sd(area) / sqrt(n)))

# POPULATION MEAN
populationMean <- ames %>%
        summarise(mu = mean(area))

# DRAW 50 SAMPLES + CALCULATE CONFIDENCE INTERVALS FOR EACH SAMPLE
ci_95 <- ames %>%
        rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
        summarise(lower = mean(area) - z_star95 * (sd(area) / sqrt(n)),
                  upper = mean(area) + z_star95 * (sd(area) / sqrt(n)))

# CREATE NEW Y/N VARIABLE IF POPULATION MEAN (MU) IS IN CONFIDENCE INTERVAL
ci_95 <- ci_95 %>%
        mutate(capture_mu = ifelse(lower < populationMean$mu & upper > populationMean$mu, "yes", "no"))

# CREATE NEW DATAFRAME TO ORGANIZE WHICH CONFIDENCE INTERVALS CONTAIN MU + WHICH DO NOT
ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci_95$lower, ci_95$upper),
                      capture_mu = c(ci_95$capture_mu, ci_95$capture_mu))

# PLOT ALL CONFIDENCE INTERVALS
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id,
                           group = ci_id, color = capture_mu)) +
        geom_point(size = 2) +
        geom_line() + 
        geom_vline(xintercept = populationMean$mu, color = 'darkgray') +
        labs(x = 'Confidence Interval Bounds',
             y = 'Confidence Interval ID')
