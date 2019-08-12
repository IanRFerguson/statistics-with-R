library(statsr)
library(dplyr)
library(ggplot2)

data(atheism)

# CONSTRUCT DF FOR 2012 AMERICAN ATHEISTS
us12 <- atheism %>%
        filter(nationality == "United States" , atheism$year == "2012")

# OBSERVE PROPORTION OF AMERICAN RESPONDENTS IDENTIFYING AS ATHEIST
us12 %>%
        summarise(athProp = (sum(us12$response == 'atheist')) / n())

# CONSTRUCT CONFIDENCE INTERVAL FOR PROPORTION OF 2012 AMERICAN ATHEISTS 
inference(y = response, data = us12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

# OBSERVE ALL NATIONALITIES SAMPLED
unique(atheism$nationality)

# CONSTRUCT DF FOR 2012 VIETNAMESE ATHEISTS
viet <- atheism %>%
        filter(nationality == 'Vietnam', year == '2012')

# CONSTRUCT CONFIDENCE INTERVAL FOR PROPORTION OF 2012 VIETNAMESE ATHEISTS
inference(y = response, data = viet, statistic = 'proportion', type = 'ci', method = 'theoretical', success = 'atheist')

# CONSTRUCT DF FOR 2012 SPANISH ATHEISTS
spain <- atheism %>%
        filter(nationality == 'Spain', year == '2012')

# CONSTRUCT CI FOR SPANISH ATHEISTS
inference(y = response, data = spain, statistic = 'proportion', type = 'ci', method = 'theoretical', success = 'atheist')

# CONSTRUCT DF FOR AMERICAN ATHEISTS REGARDLESS OF YEAR
unidos <- atheism %>%
        filter(nationality == 'United States')

# COMPARE SAMPLE YEARS
unidos %>%
        group_by(year) %>%
        summarise(atheists = (sum(response == 'atheist')), sample = n())

# OBSERVE SIGNIFICANT DIFFERENCES IN 2005 AND 2012 RESPONSES
inference(y = response, x = year, data = unidos, type = 'ht', statistic = 'proportion', 
          success = 'atheist', method = 'theoretical', alternative = 'twosided')
