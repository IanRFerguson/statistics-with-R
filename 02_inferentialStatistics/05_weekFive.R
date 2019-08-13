library(ggplot2)
library(dplyr)
library(statsr)

load("gss.Rdata")

# Is gun ownership linked to support for the government? Specifically in terms of an increased standard of living?

# DEFINE WORKING DATA SET
workingData <- gss[!gss$owngun == 'NA', ]
workingData <- workingData[!workingData$owngun == 'Refused', ]
workingData <- workingData[!workingData$helpnot == 'Agree With Both', ]

# GROUP BY GUN OWNERSHIP, SUMMARIZE BY POLITICAL OPINION
workingData %>%
        group_by(owngun) %>%
        summarise(sample = n(), 
                  supportGov = (sum(helpnot == 'Govt Do More', na.rm = T)), 
                  rejectGov = (sum(helpnot == 'Govt Does Too Much', na.rm = T)))

# SUMMARIZE BY PROPORTION
workingData %>%
        group_by(owngun) %>%
        summarise(sample = n(), 
                  supportProportion = (sum(helpnot == 'Govt Do More', na.rm = T) / n()), 
                  rejectProportion = (sum(helpnot == 'Govt Does Too Much', na.rm = T) / n()))

# PLOT
ggplot(data = subset(workingData, !is.na(helpnot)), aes(x = helpnot, fill = owngun)) +
        geom_bar() + 
        labs(x = 'Support for Government',
             title = 'Gun Ownership vs. Support for Government')

# HYPOTHESIS TEST FOR GROUP DIFFERENCES + REJECT THE NULL
inference(y = helpnot, x = owngun, data = workingData, type = 'ht', statistic = 'proportion', 
          success = 'Govt Do More', method = 'theoretical', alternative = 'greater')
