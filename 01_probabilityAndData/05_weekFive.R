library(ggplot2)
library(dplyr)
load("brfss2013.RData")

# Does fruit and vegetable consumption differ reliably by state?
exploreNutrition <- data.frame(brfss2013$X_state, brfss2013$X_frutsum, brfss2013$X_vegesum)
exploreNutrition <- exploreNutrition[complete.cases(exploreNutrition), ]
colnames(exploreNutrition) <- c('state','fruitSum','vegSum')

exploreNutrition <- exploreNutrition %>%
        mutate(nutrientIntake = fruitSum + vegSum)

# Low to High
exploreNutrition %>%
        group_by(state) %>%
        summarise(averageNutrientIntake = mean(nutrientIntake), n = n()) %>%
        arrange(averageNutrientIntake)

# High to Low
exploreNutrition %>%
        group_by(state) %>%
        summarise(averageNutrientIntake = mean(nutrientIntake), n = n()) %>%
        arrange(desc(averageNutrientIntake))

ggplot(data = exploreNutrition, aes(x = factor(state), y = nutrientIntake)) + 
        geom_boxplot() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Does income predict poor physical or mental health?
exploreIncome <- data.frame(brfss2013$X_state, brfss2013$employ1, brfss2013$income2, brfss2013$poorhlth)
colnames(exploreIncome) <- c('state','employmentStatus','incomeLevel','poorHealth')

exploreIncome <- exploreIncome[complete.cases(exploreIncome), ]

exploreIncome %>%
        group_by(incomeLevel) %>%
        summarise(averagePoorHealthDays = mean(poorHealth)) %>%
        arrange(desc(averagePoorHealthDays))

ggplot(exploreIncome, aes(x = incomeLevel, y = poorHealth)) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

# Does partisan legislative control predict health outcomes for their constituents?
browseURL('https://ballotpedia.org/Partisan_composition_of_state_legislatures')

blueStates <- c('Washington','Oregon','California','Nevada','Colorado','New Mexico',
                'Illinois','Maryland','Delaware','New Jersey','New York','Connecticut',
                'Massachusets','Rhoad Island','Vermont','New Hampshire','Maine')

explorePolitics <- data.frame(brfss2013$X_state, brfss2013$hlthcvrg, brfss2013$lstcovrg, brfss2013$medcost, brfss2013$carercvd)
colnames(explorePolitics) <- c('state','healthCare','timeSinceHealthCare','medCost','careReceived')
explorePolitics <- explorePolitics[explorePolitics$state != c('Puerto Rico','Guam'), ]
explorePolitics <- explorePolitics[complete.cases(explorePolitics), ]

explorePolitics <- explorePolitics %>%
        mutate(statePolitics = ifelse(test = (explorePolitics$state %in% blueStates), yes = 'Democrat', no = 'Republican'))

explorePolitics %>%
        group_by(statePolitics) %>%
        summarise(satisfaction = sum(careReceived == 'Very satisfied'), satisfactionRate = (satisfaction / n()), n = n()) %>%
        arrange(desc(satisfaction))

ggplot(explorePolitics, aes(x = careReceived, fill = statePolitics)) + 
        geom_bar()
