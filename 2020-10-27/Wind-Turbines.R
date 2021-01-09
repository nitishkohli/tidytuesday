library(tidyverse)
library(here)
library(tidytuesdayR)
library(xts)
library(dygraphs)
library(streamgraph)

data <- tt_load('2020-10-27')
wind.turbine <- data$`wind-turbine`

top_10_manuf <- wind.turbine %>%
  group_by(manufacturer) %>%
  summarise(cap = sum(turbine_rated_capacity_k_w),
            .groups = 'drop') %>%
  slice_max(n = 10, order_by = cap)

wind.turbine %>%
  #filter(manufacturer %in% top_10_manuf$manufacturer) %>%
  select(commissioning_date, manufacturer, turbine_rated_capacity_k_w) %>%
  mutate(commissioning_date = str_sub(commissioning_date, 1, 4) %>% as.numeric()) %>%
  complete(commissioning_date, manufacturer) %>%
  mutate(turbine_rated_capacity_k_w = replace_na(turbine_rated_capacity_k_w, 0)) %>%
  arrange(commissioning_date, manufacturer) %>%
  group_by(commissioning_date, manufacturer) %>%
  summarise(capacity = sum(turbine_rated_capacity_k_w),
            .groups = 'drop') %>%
  group_by(manufacturer) %>%
  mutate(capacity_cum = cumsum(capacity)/1000,
         commissioning_date = as.Date(paste(commissioning_date, '01', '01', sep = '-'))) %>%
  select(-capacity) %>%
  arrange(-capacity_cum) %>%
  pivot_wider(names_from = manufacturer, values_from = capacity_cum, values_fn = sum) %>%
  xts(x = .[,-1], order.by = .$commissioning_date) %>%
  dygraph() %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector() %>%
  dyLegend(width = "100%")


