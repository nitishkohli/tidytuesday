library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggfittext)
library(ggchicklet)
library(cowplot)
library(magrittr)

tuesdata <- tidytuesdayR::tt_load('2021-02-02')

hbcu <- bind_rows(select(tuesdata$hbcu_all, Year:Females) %>% mutate(Categ = 'All'),
                  select(tuesdata$hbcu_black, Year:Females) %>% mutate(Categ = 'Black')) %>%
  janitor::clean_names()

hbcu <- hbcu %>%
  pivot_longer(cols = -c(year, categ), names_to = 'gender') %>%
  pivot_wider(names_from = categ) %>%
  mutate(Others = All - Black) %>%
  select(-All) %>%
  pivot_longer(cols = -c(year, gender), names_to = 'categ')

hbcu %>%
  filter(gender == 'total_enrollment') %>%
  group_by(year, gender) %>%
  mutate(perc = value / sum(value)) %>%
  ungroup() %>%
  ggplot(aes(x = as.character(year), y = value, fill = reorder(categ, desc(categ)),
             ymin = 0, ymax = value)) +
  geom_col(position = 'fill') +
  geom_fit_text(aes(label = percent(perc, accuracy = 1)),
                position = 'fill', place = 'top',
                contrast = T, min.size = 3, show.legend = F) +
  scale_fill_manual(values = c('Black' = '#bdbdbd', 'Others' = '#feb062')) +
  guides(fill = guide_legend(reverse=T)) +
  coord_polar() +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#2b2b2b"),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = 'transparent'),
        legend.title = element_blank(),
        legend.position = 'top',
        legend.justification = 'right',
        legend.margin = margin(t = 0, r = 0, b = -25, l = 0),
        legend.text = element_text(color = '#f0f0f0'),
        axis.text.x = element_text(color = '#f0f0f0'),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank())

p1 <- tuesdata$bach_students %>%
  mutate(categ = 'Bach') %>%
  bind_rows(tuesdata$hs_students) %>%
  mutate(categ = replace_na(categ, 'HS')) %>%
  rename(Year = Total) %>%
  select(-c(matches('Standard Error|percent|Total'))) %>%
  pivot_longer(cols = -c(Year, categ), names_to = 'Ethnicity', values_to = 'n') %>%
  mutate(Ethnicity = str_squish(Ethnicity),
         Ethnicity = str_replace_all(Ethnicity, '[0-9]', ''),
         n = as.numeric(n) %>% replace_na(0)) %T>%
  { test <<- . } %>%
  filter(n != 0) %>%
  ggplot(aes(x = as.character(Year), y = n, fill = Ethnicity)) +
  geom_col(color = 'black', position = 'stack') +
  scale_fill_brewer(palette = 'Greys', direction = -1) +
  facet_wrap(~categ) +
  coord_polar(theta = 'x') +
  theme_void() 

p1
