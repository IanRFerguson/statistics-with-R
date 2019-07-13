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

# LOAD DATA INTO ENVIRONMENT
attach(present)
# ADD TOTAL COLUMN
present <- present %>%
        mutate(total = boys + girls)
# ADD LOGICAL 'BOYS > GIRLS' COLUMN (T/F)
present <- present %>%
        mutate(more_boys = boys > girls)
# ADD PROPORTION OF BOYS / GIRLS COLUMN
present <- present %>%
        mutate(prop_boy_girl = boys / girls)
# GET HIGHEST BIRTH RATE YEAR
present <- present[order(present$total, decreasing = TRUE), ]
present$year[1]
