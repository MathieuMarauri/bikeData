
# Exploratory analysis is performed. The aim is to find useful insight on the
# data regarding its structure (linked between the dependant variable and the
# predictors, ...)

# Inputs: the cleaned data

# Outputs: insights and plots + bikes data with new variables (time variables).

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('ggplot2') # data visualization
library('lubridate') # time object manipulation
library('extrafont') # import fonts


# The dependant variable --------------------------------------------------

# First the dependant variable (count) is analysed. How is it distributed
# accross time?

# Evolution of number of bikes rented over time. Initiate a complete sequence of
# datetime between the min and the max, merge with the data and plot the result.
# If there are missing dates it will be clear.

# import data
bikes <- readRDS('data/clean/bikes.rds')

# initiate sequence of datetime 
datetime_sequence <- seq(from = min(bikes$datetime),
                         to = max(bikes$datetime),
                         by = "hour")

# merge bikes data with the complete sequence of datetime and plot the count variable
bikes_complete <- merge(x = as.data.table(datetime_sequence),
                        y = bikes,
                        all.x = TRUE,
                        by.x = "x",
                        by.y = "datetime")

# fill missing values with 0
bikes_complete$count[is.na(bikes_complete$count)] <- 0

# plot the result
ggplot(data = bikes_complete, mapping = aes(x = x, y = count)) + 
  geom_line()

# To properly see the data that seems missing create a date variable and
# aggreagate the number of bikes by day. Merge the resulting table with a
# complete sequence of dates and plot the result.

# creation of the date variable
bikes[ , date := as.Date(datetime, format = '%Y-%m-%d', tz = 'CET')]

# aggragtion by day
bikes_complete <- bikes[, .(count = sum(count)), by = date]

# complete sequence of days 
date_sequence <- seq(from = min(bikes_complete$date), 
                     to = max(bikes_complete$date),
                     by = 'day')

# merge with the aggregated number of bikes rented
bikes_complete <- merge(x = as.data.table(date_sequence),
                        y = bikes_complete,
                        all.x = TRUE,
                        by.x = "date_sequence",
                        by.y = "date")

# fill missing values with 0
bikes_complete$count[is.na(bikes_complete$count)] <- 0

# plot the result
ggplot(data = bikes_complete, mapping = aes(x = date_sequence, y = count)) + 
  geom_line() + 
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b - %Y") + 
  labs(x = 'Date', y = 'Nombre de vélos loués', title = 'Evolution du nombre de vélos loués')

# clean session
rm(datetime_sequence, date_sequence, bikes_complete)

# Data is missing for every day after the 19th of each month. 


# Time variables ----------------------------------------------------------

# Specific time variables are created here to catch the seasonality observed in
# the data. Indicators of year, month, day of the month, day of the week and
# hour of the day are created. The number of bikes rented by levels of each new
# factor is then analysed to see if there is indeed a seasonality on these
# different levels. The aim is to be able to find the variables that have
# influence on the number of rips rented. For each variable a plot and an
# kruskal test are analysed to detect the impact. This test is used as the
# non-parametric equivalent of the anova since the distributions are not normal.

# creation of the new variables
bikes[, year := year(date)]
bikes[, month := month(date, label = TRUE, abbr = FALSE)]
bikes[, day_month := mday(date)]
bikes[, day_week := wday(date, label = TRUE, abbr = FALSE)]
bikes[, hour_day := hour(datetime)]

# influence of the month
ggplot(data = bikes[, .(count = mean(count)), by = month], 
       mapping = aes(x = month, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes, mapping = aes(x = count, fill = month)) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$month)

# influence of the day of the month 
ggplot(data = bikes[, .(count = mean(count)), by = day_month], 
       mapping = aes(x = day_month, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes, mapping = aes(x = count, fill = as.factor(day_month))) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$day_month)

# influence of the day of the week
ggplot(data = bikes[, .(count = mean(count)), by = day_week], 
       mapping = aes(x = day_week, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes, mapping = aes(x = count, fill = day_week)) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$day_week)

# influence of the hourof the day
ggplot(data = bikes[, .(count = mean(count)), by = hour_day], 
       mapping = aes(x = hour_day, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes, mapping = aes(x = count, fill = as.factor(hour_day))) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$hour_day)

# The different variables have all an effect on the number of bikes rented. The
# day of the month and the day of the week effect is less clear.


# Time related variables --------------------------------------------------

# The variables analysed here are holiday, workingday and season. They are
# closely linked to the date but give extra information and may regroup several
# dates.

# influence of the season
ggplot(data = bikes[, .(count = mean(count)), by = season], 
       mapping = aes(x = season, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes, mapping = aes(x = count, fill = season)) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$season)

# influence of the day being an holiday
ggplot(data = bikes[, .(count = mean(count)), by = holiday], 
       mapping = aes(x = holiday, y = count)) + 
  geom_bar(stat = 'identity')

t.test(x = bikes[holiday == 1]$count, y = bikes[holiday == 0]$count)

# influence of the day being an working-day
ggplot(data = bikes[, .(count = mean(count)), by = workingday], 
       mapping = aes(x = workingday, y = count)) + 
  geom_bar(stat = 'identity')

t.test(x = bikes[workingday == 1]$count, y = bikes[workingday == 0]$count)

# influence of the day being a working day combined with the hour of the day
ggplot(data = bikes[, .(count = mean(count)), by = list(workingday, hour_day)], 
       mapping = aes(x = hour_day, y = count, fill = workingday)) + 
  geom_bar(stat = 'identity', position = 'dodge')

anova <- aov(formula = count ~ hour_day * workingday, data = bikes)
summary(anova)

# cleans session
rm(anova)

# The information behind holiday can be retreived in workingday. The fact that a
# day is working day alone does not seem to have an impact on the number of
# bikes rented. But the interaction with the hour of the day has a clear impact.
# The season is also important. The information behind is closely related to the
# month but is somewhat more precise as seasons start and end in middle of
# months. On the other hand the mnths of spring are really different in terms of
# bikes rented.


# Weather variables -------------------------------------------------------

# Weather data is analysed in this section. The temperature, the humidy and the
# windspeed are analysed using scatter plot and linear regression if a linear
# realtion appears. The weather variable which indicates the global weather is
# analysed as using plot and anova as before.

# influence of the global weather
ggplot(data = bikes[, .(count = mean(count)), by = weather], 
       mapping = aes(x = weather, y = count)) + 
  geom_bar(stat = 'identity')

ggplot(data = bikes[weather != 4], mapping = aes(x = count, fill = weather)) +
  geom_density(alpha = 0.5)

kruskal.test(x = bikes$count, g = bikes$weather)

# influence of the temperature
ggplot(data = bikes, mapping = aes(x = temp, y = count)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = 'gam')

regression <- lm(formula = count ~ temp, data = bikes)
summary(regression)

# influence of the apparent temperature
ggplot(data = bikes, mapping = aes(x = atemp, y = count)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = 'gam')

regression <- lm(formula = count ~ atemp, data = bikes)
summary(regression)

# influence of the humidity
ggplot(data = bikes, mapping = aes(x = humidity, y = count)) + 
  geom_point() + 
  geom_smooth(method = 'gam')

regression <- lm(formula = count ~ humidity, data = bikes)
summary(regression)

# influence of the wind speed
ggplot(data = bikes, mapping = aes(x = windspeed, y = count)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = 'gam')

regression <- lm(formula = count ~ windspeed, data = bikes)
summary(regression)

# The weather variables have an impact on the number of bikes rented as it is
# expected but this impact is not strong. 

# clean session
rm(regression)


# Nice plots --------------------------------------------------------------

# Plots for the output are tuned. The selected plots are the ones with the month
# variable, the hour the day variable, the season, the combination of working
# day and hour of the day and the temperature. These variables are the ones
# showing the strongest relation to the number of bikes rented.

# load font to add to ggplot
windowsFonts(century = "Century Gothic")
loadfonts(device = "win")

# create the global theme for the plots
theme_plot <- theme_bw() +
  theme(text = element_text(family = 'century'), 
        plot.title = element_text(size = 30, colour = 'grey10', face = 'bold'),
        plot.subtitle = element_text(size = 20, colour = 'grey10', face = 'italic'),
        axis.title = element_text(size = 25, colour = 'grey10'),
        axis.text = element_text(size = 20, colour = 'grey10'))

# number of bikes over time
ggplot(data = bikes_complete, mapping = aes(x = date_sequence, y = count)) + 
  geom_line(color = 'dodgerblue4') + 
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b") + 
  labs(x = 'Date', y = 'Nombre de vélos loués', title = 'Evolution du nombre de vélos loués') +
  theme_plot

# temperature
ggplot(data = bikes, mapping = aes(x = atemp, y = count)) + 
  geom_point(alpha = 0.5, size = 3, fill = 'dodgerblue4', colour = 'dodgerblue4') + 
  geom_smooth(method = 'lm', colour = 'maroon4', fill = 'maroon4') + 
  labs(x = 'Température ressentie', y = 'Nombre de vélos loués', 
       title = 'Nombre moyen de vélos loués par heure en fonction \nde la température ressentie',
       subtitle = 'La courbe est obtenue avec un modèle linéaire, p-value ~ 0') + 
  theme_plot

# month
ggplot(data = bikes[, .(count = mean(count), nb = .N), by = month], 
       mapping = aes(x = month, y = count)) + 
  geom_bar(stat = 'identity', fill = 'dodgerblue4') + 
  # geom_text(mapping = aes(x = month, y = 10, label = nb), 
  #           family = 'century', size = 10, colour = 'white') +
  geom_text(mapping = aes(x = 1, y = mean(bikes$count) + 10, label = round(mean(bikes$count))),
            size = 7, colour = 'maroon4', family = 'century') + 
  geom_hline(mapping = aes(yintercept = mean(bikes$count)), color = 'maroon4', size = 2) + 
  labs(x = '', y = 'Nombre de vélos loués', 
       title = 'Nombre moyen de vélos loués par heure \nen fonction du mois.') + 
  theme_plot + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# hour of the day
ggplot(data = bikes[, .(count = mean(count), nb = .N), by = hour_day], 
       mapping = aes(x = hour_day, y = count)) + 
  geom_bar(stat = 'identity', fill = 'dodgerblue4') + 
  # geom_text(mapping = aes(x = month, y = 10, label = nb), 
  #           family = 'century', size = 10, colour = 'white') +
  geom_text(mapping = aes(x = 0, y = mean(bikes$count) + 15, label = round(mean(bikes$count))),
            size = 7, colour = 'maroon4', family = 'century') + 
  geom_hline(mapping = aes(yintercept = mean(bikes$count)), color = 'maroon4', size = 2) + 
  scale_x_continuous(breaks = 0:23) + 
  labs(x = 'Heure de la journée', y = 'Nombre de vélos loués', 
       title = 'Nombre moyen de vélos loués par heure.') + 
  theme_plot

# season
ggplot(data = bikes[, .(count = mean(count), nb = .N), by = season], 
       mapping = aes(x = season, y = count)) + 
  geom_bar(stat = 'identity', fill = 'dodgerblue4') + 
  # geom_text(mapping = aes(x = season, y = 10, label = nb),
  #           family = 'century', size = 10, colour = 'white') +
  geom_text(mapping = aes(x = 0.5, y = mean(bikes$count) + 10, label = round(mean(bikes$count))),
            size = 7, colour = 'maroon4', family = 'century') +
  geom_hline(mapping = aes(yintercept = mean(bikes$count)), color = 'maroon4', size = 2) + 
  scale_x_discrete(breaks = 1:4, labels = c('Hiver', 'Printemps', 'Été', 'Automne')) +
  labs(x = 'Saison', y = 'Nombre de vélos loués', 
       title = 'Nombre moyen de vélos loués par heure en fonction de la saison.') + 
  theme_plot

# workinkday * hour of the day
ggplot(data = bikes[, .(count = mean(count)), by = list(workingday, hour_day)], 
       mapping = aes(x = hour_day, y = count, fill = workingday)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_text(mapping = aes(x = 0, y = mean(bikes$count) + 15, label = round(mean(bikes$count))),
            size = 7, colour = 'maroon4', family = 'century') + 
  geom_hline(mapping = aes(yintercept = mean(bikes$count)), color = 'maroon4', size = 2) + 
  scale_fill_manual(breaks = 0:1, values = c('dodgerblue4', 'navajowhite4'), 
                    labels = c('Non', 'Oui'), name = 'Journée travaillée') + 
  scale_x_continuous(breaks = 0:23) + 
  labs(x = 'Heure de la journée', y = 'Nombre de vélos loués', 
       title = 'Nombre moyen de vélos loués par heure en fonction \ndu type de journée : travaillée ou non.',
       subtitle = 'Environ 2/3 des jours sont travaillés.') + 
  theme_plot + 
  theme(legend.position = c(0.15, 0.9), 
        legend.background = element_rect(colour = 'grey20'),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25))

# save results and clean session
saveRDS(bikes, 'data/clean/bikes_clean.rds')
rm(theme_plot, bikes)
