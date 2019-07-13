# //////////////////// WEEK ONE ////////////////////////
library(dplyr)
library(ggplot2)
library(statsr)

# LOAD DATA INTO ENVIRONMENT
data(arbuthnot)

# PLOT GIRLS x YEAR
ggplot(data = arbuthnot, aes(x = year, y = girls)) + geom_point()

# ADD 'TOTAL' COLUMN
arbuthnot <- arbuthnot %>%
        mutate(total = boys + girls)

# PLOT TOTAL x YEAR
ggplot(data = arbuthnot, aes(x = year, y = total)) + geom_point()
# PLOT PROPORTION OF BOYS x YEAR
ggplot(data = arbuthnot, aes(x = year, y = boys / total)) + geom_point()

# ADD LOGICAL 'BOYS > GIRLS' COLUMN (T/F)
arbuthnot <- arbuthnot %>%
        mutate(more_boys = boys > girls)

