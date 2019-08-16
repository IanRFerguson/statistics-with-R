library(statsr)
library(dplyr)
library(ggplot2)

data(mlb11)

# PLOT RUNS x AT BATS
ggplot(data = mlb11, aes(x = runs, y = at_bats)) +
        geom_point()

# OBSERVE CORRELATION BETWEEN RUNS x AT BATS
mlb11 %>%
        summarise(correlation = cor(runs, at_bats))

# CONSTRUCT LINEAR MODEL PREDICTING RUNS x AT BATS
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

# CONSTRUCT LINEAR MODEL PREDICTING x HOMERUNS
batterUp <- lm(runs ~ homeruns, data = mlb11)
summary(batterUp)

# PLOT AT BATS x RUNS + ADD LINE OF BEST FIT
ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
        geom_point() +
        stat_smooth(method = "lm", se = FALSE)

# USE MODEL TO PREDICT NO. OF RUNS + CALCULATE RESIDUALS
mlb11 %>%
        filter(at_bats == 5579) %>%
        select(runs)

newModel <- lm(at_bats ~ runs, data = mlb11)
summary(newModel)
expectedRuns <- 5113.3510 + (0.5913 * 713)
observedRuns <- 5579

residualRuns <- observedRuns - expectedRuns
residual <- observedRuns - expectedRuns

# PLOT RESIDUALS
ggplot(data = m1, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        xlab("Fitted values") +
        ylab("Residuals")

ggplot(data = m1, aes(x = .resid)) +
        geom_histogram(binwidth = 25) +
        xlab("Residuals")

# NORMAL PROBABILITY PLOT
ggplot(data = m1, aes(sample = .resid)) +
        stat_qq()

# FIND STAT CLOSELY ASSOCIATED WITH RUNS
mlb11 %>%
        summarise(runsAtBats = cor(runs, at_bats),
                  runsHits = cor(runs, hits),
                  runsWins = cor(runs, wins),
                  runsBatAvg = cor(runs, bat_avg))

mlb11 %>%
        summarise(onBase = cor(runs, new_obs),
                  slugging = cor(runs, new_slug),
                  onBasePer = cor(runs, new_onbase))
