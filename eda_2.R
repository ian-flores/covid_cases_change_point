library(tidyverse)

covid_daily <- read_csv('COVID-19 Daily Cases - Sheet1.csv') %>%
  filter(date >= '2020-03-09')

library(lubridate)

wrangled_data <- covid_daily %>%
  mutate(day_num = wday(date),
         weekend = if_else(day_num %in% c(1, 7), '1', '0'))
  

library(infer)

obs_t <- wrangled_data %>%
  specify(cases ~ weekend) %>%
  calculate(stat = 't', order = c('0', '1'))

t_null_perm <- wrangled_data %>%
  specify(cases ~ weekend) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 10000, type = 'permute') %>%
  calculate(stat = 't', order = c('0', '1'))

visualize(t_null_perm) +
  shade_p_value(obs_stat = obs_t, direction = "two_sided") +
  labs(title = 'Difference in weekend vs weekday in the date of \npublication of COVID-19 results in PR (Simulation based)',
       subtitle = glue::glue('p-value: ', {
         t_null_perm %>%
           get_p_value(obs_stat = obs_t, direction = "two_sided") %>%
           pull(p_value)
       }))

t_null_perm %>%
  get_p_value(obs_stat = obs_t, direction = "two_sided")

wrangled_data %>%
  ggplot() +
    geom_histogram(aes(cases), binwidth = 5) +
    facet_wrap(~ weekend)

wrangled_data %>%
  group_by(weekend) %>%
  summarize(mu = mean(cases, na.rm=T),
            sd = sd(cases, na.rm=T),
            n = n())