library(statsr)
library(dplyr)
library(ggplot2)

data(nc)

# OBSERVE DIFFERENCES IN SAMP DISTRIBUTION BETWEEN SMOKING AND NON-SMOKING MOTHERS
ggplot(data = nc, aes(x = habit, y = weight)) + geom_boxplot()

# VIEW MEAN DIFFERENCES IN BIRTHWEIGHT BETWEEN CHILDREN OF SMOKERS / NON-SMOKERS
nc %>%
        group_by(habit) %>%
        summarise(mean_weight = mean(weight, na.rm = T))

# HYPOTHESIS TESTING WITH STATSR PACKAGE

# TEST FOR MEAN DIFFERENCES IN BIRTHWEIGHT BETWEEN CHILDREN OF SMOKERS / NON-SMOKERS
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

# CONSTRUCT CONFIDENCE INTERVAL (DEFUALT == 95%)
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", null = 0, 
          alternative = 'twosided', method = 'theoretical')

# CONSTRUCT SAME CI AT 0.9 CONFIDENCE LEVEL
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", conf_level = 90, null = 0, 
          alternative = 'twosided', method = 'theoretical')

# VIEW MEAN / MIN / MAN FOR 'YOUNG' VS. 'MATURE' MOTHERS IN SAMPLE
nc %>%
        group_by(mature) %>%
        summarise(mean = mean(mage), min = min(mage), max = max(mage))

# TEST FOR MEAN DIFFERENCES IN BIRTHWEIGHT FOR WHITE / NON-WHITE MOTHERS
inference(y = weight, x = whitemom, data = nc, type = 'ht', null = 0, 
          statistic = 'mean', alternative = 'twosided', method = 'theoretical')
