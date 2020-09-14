library(tidytuesdayR)
library(tidyverse)

friends <- tt_load('2020-09-08')

friends1 <- friends$friends

friends1 %>%
  filter(speaker == '#ALL#') %>%
  count(speaker, season) %>%
  ggplot(aes(x = speaker, y = n)) +
  geom_col() +
  facet_wrap(~season)
