library(tidyverse)

load("data/clean_datasets.Rdata")


head(general_info, 5)

summary(general_info)

head(similar_artists, 7)
# Rank- 1 is the most similar

head(tags, 7)
# Rank- 1 is the most similar

# Test my popularity vs similar artists ----
similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name) %>%
  left_join(general_info, by = c("similar" = "name")) %>%
  select(-listeners, -rank) %>%
  group_by(artist) %>%
  summarise(med_playcount = median(playcount, na.rm = T),
            mean_playcount = mean(playcount, na.rm = T)) -> similar_agg


general_info %>%
  select(-listeners) %>%
  left_join(similar_agg, by = c("name" = "artist")) -> both


both %>%
  mutate(my_others_ratio = playcount/mean_playcount) %>%
  summary()

# Artists recommended as similar have 20x bigger playcount

