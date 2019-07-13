library(statsr)
library(dplyr)
library(ggplot2)

# LOAD DATA INTO ENVIRONMENT
data("kobe_basket")
attach(kobe_basket)

# CALCULATE STREAK OF KOBE'S SHOTS (I.E., CONSECUTIVE SHOTS BEFORE MISS)
kobe_streak <- calc_streak(kobe_basket$shot)

# PLOT LENGTH OF HEAT CHECK
ggplot(data = kobe_streak, aes(x = length)) + geom_histogram()

# /////////// SIMULATIONS IN R /////////////
coin_outcomes <- c('heads', 'tails')
sample(coin_outcomes, size = 1, replace = TRUE)

# 100 COIN FLIPS
simulateFairCoin <- sample(coin_outcomes, size = 100, replace = TRUE)
# SUMMARY TABLE
table(simulateFairCoin)

# HIGHWAY ROBBERY = UNFAIR COIN
simulateUnfairCoin <- sample(coin_outcomes, size = 100, replace = TRUE, prob = c(0.25, 0.75))
# SUMMARY TABLE
table(simulateUnfairCoin)

# /////////// SIMULATED HEAT CHECK /////////////
shotOutcomes <- c('H', 'M') # H == Hit | M == Miss
simulatedShot <- sample(shotOutcomes, size = 1, replace = TRUE)

# SIMULATE KOBE
simulatedKobe <- sample(shotOutcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
table(simulatedKobe)

# CALCULATE SIM KOBE STREAK
simKobeStreak <- calc_streak(simulatedKobe)

# PLOT SIM KOBE
ggplot(data = simKobeStreak, aes(x = length)) + geom_histogram()
