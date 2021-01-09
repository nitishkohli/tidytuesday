library(tidyverse)
library(lubridate)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost <- transit_cost %>%
  mutate(country = if_else(country == 'UK', 'GB', country),
         country.name = countrycode::countrycode(country, 'ecb', 'country.name.en'))

transit_cost %>%
  filter(country == 'IN') %>%
  select(country, country.name, city, line, start_year, end_year, year,
         length, tunnel, stations, real_cost, cost_km_millions) %>%
  arrange(start_year) -> test


test %>%
  filter(!is.na(start_year) & !is.na(end_year)) %>%
  mutate(start_year = ymd(paste(start_year, '01', '01', sep='-')),
         end_year = ymd(paste(end_year, '01', '01', sep='-'))) %>%
  ggplot(aes(x = start_year, y = reorder(line, desc(start_year)), group = city)) +
  geom_segment(aes(xend = end_year, yend = line)) +
  geom_point() +
  geom_point(aes(x = end_year)) +
  geom_vline(xintercept = as.Date('2020-07-01')) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  facet_wrap(~reorder(city, desc(start_year)), scales = 'free_y') +
  theme_bw() +
  theme(panel.grid = element_blank())
