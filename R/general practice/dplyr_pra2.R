library(dplyr)

# mutate, join, select, rename, sort
starwars %>% mutate(bmi = mass/(height/100)^2) %>%
  select(name, height2 = height, mass, birth_year, bmi) %>% # rename can be combined in select or separate
  left_join(starwars, by = c('name'='name', 'mass'= 'mass')) %>%
  arrange(desc(name), height) %>%
  rename(skin = skin_color, birth = birth_year.x)   # rename, new_name = current_name


msleep<-ggplot2::msleep 
glimpse(msleep)
# filter the data by multi conditions
msleep %>% 
  filter(between(sleep_total, 10, 20) & 
           order %in% c('Didelphimorphia', 'Diprotodontia') & 
           !is.na(conservation))

head(msleep)
# new group_by() will replace the previous group_by(). Or ungroup() can be used
msleep %>% group_by(vore) %>%
  filter(sleep_total > quantile(sleep_total, 0.95)) %>% # agg by different level
  group_by(genus, vore, order) %>%
  summarise(sleep_mean = mean(sleep_total),
            group_size = n()) %>%
  group_by(order) %>%
  mutate(rank = rank(sleep_mean, ties.method = 'first')) %>%
  filter(rank<2)

# arregate by condition
msleep %>% group_by(vore) %>%
  summarise(count = sum(sleep_total>mean(sleep_total)))

# calculation is fixed in vore level, but return the original dimension
msleep %>% group_by(vore) %>%
  mutate(voure_count = sum(sleep_total>mean(sleep_total)),
         sleep_norm = sleep_total/mean(sleep_total),
         sleep_mean = mean(sleep_total)) %>%
  select(vore, voure_count, sleep_total, sleep_mean, sleep_norm)
