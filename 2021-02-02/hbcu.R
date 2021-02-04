library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggfittext)
library(ggchicklet)
library(cowplot)

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


p1 <- hbcu %>%
  filter(gender != 'total_enrollment') %>%
  group_by(year, gender) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  mutate(gender = factor(gender, levels = c('males', 'females'))) %>%
  ggplot(aes(x = as.character(year), y = value, fill = gender,
             ymin = 0, ymax = value)) +
  geom_chicklet(position = 'stack') +
  geom_fit_text(aes(label = label_number_si(accuracy = 1)(value)),
                position = 'stack', place = 'top',
                contrast = T, min.size = 3, show.legend = F) +
  scale_fill_manual(values = c('females' = '#d175b7', 'males' = '#1c2c54')) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#f0f0f0"),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = 'transparent'),
        legend.title = element_blank(),
        legend.position = 'top',
        legend.justification = 'right',
        legend.margin = margin(t = 0, r = 0, b = -25, l = 0),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())


p2 <- hbcu %>%
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

p1 + p2

  
