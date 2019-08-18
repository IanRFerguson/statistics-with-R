install.packages('GGally')

library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

data(evals)

# OBSERVE SUMMARY STATISTICS FOR SCORE VARIABLE
summary(evals$score)

# PLOT DISTRIBUTION OF MINORITY + NON-MINORITY PROFESSORS
ggplot(data = evals, aes(x = ethnicity, y = bty_avg)) +
        geom_boxplot()

# PLOT CORRELATION BETWEEN BEAUTY RATING AND TEACHING RATING
ggplot(data = evals, aes(x = bty_avg, y = score)) +
        geom_jitter()

# SAME PLOT + LINE OF BEST FIT
ggplot(data = evals, aes(x = bty_avg, y = score)) +
        geom_jitter() +
        geom_smooth(method = "lm", se = FALSE)

# CONSTRUCT LINEAR REGRESSION MODEL + OBSERVE FORMULA
beautyModel <- lm(bty_avg ~ score, data = evals)
summary(beautyModel)

# PLOT RESIDUALS FROM ABOVE LINEAR MODEL
ggplot(data = beautyModel, aes(x = .fitted, y = .resid)) +
        geom_point() + 
        geom_hline(yintercept = 0)

# PLOT CORRELATION BETWEEN AVERAGE BEAUTY AND SCORES PROVIDED BY FEMALE LOWER STUDENTS
ggplot(data = evals, aes(x = bty_f1lower, y = bty_avg)) +
        geom_jitter()

# OBSERVE CORRELATION
evals %>% 
        summarise(cor(bty_avg, bty_f1lower))

# CONSTRUCT MULTIPLE LINEAR REGRESSION MODEL + OBSERVE FORMULA
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

# PLOT RESIDUALS FROM THE MODEL ABOVE
ggplot(data = m_bty_gen, aes(x = .resid)) +
        geom_histogram()
