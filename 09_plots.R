library(gamlss.dist)
library(tidyverse)
library(igraph)
library(fitdistrplus)
# install.packages("kSamples")

load("data/simulation_results.Rdata")

similar_graph <- results_list[[1]]

playcount <- vertex_attr(similar_graph)$playcount
simulation_playcount <- vertex_attr(similar_graph)$simulation_playcount
simulation_playcount_scaled <- simulation_playcount/max(simulation_playcount)*max(playcount)

#### Fit yule
mle_yule <-
  fitdistrplus::mledist(vertex_attr(similar_graph)$playcount,
                        "YULE",
                        start = list("mu" = 1000))
mle_yule$estimate # 1898.966 - słabo pasuje

# simulation_yule <-
#   rYULE(9998, mu = mle_yule$estimate)


#### Quantile-quantile plot

quantiles_simulation <- quantile(simulation_playcount, probs = seq(0,0.99,0.01))
quantiles_simulation <- as.numeric(quantiles_simulation)
quantiles_empirical <- quantile(playcount, probs = seq(0,0.99,0.01))
quantiles_yule <- qYULE(seq(0,0.99,0.01), mu = 10000)
# quantiles_yule <- quantiles_yule/max(quantiles_yule)*max(playcount)

tibble(
  p = 1:100,
  quantiles_empirical,
       quantiles_simulation, 
       quantiles_yule
       ) %>%
  mutate(quantiles_yule = quantiles_yule/max(quantiles_yule)*max(quantiles_empirical),
         quantiles_simulation = quantiles_simulation/max(quantiles_simulation)*max(quantiles_empirical)) %>%
  rename(`Simulation` = quantiles_simulation,
         `Fitted Yule distribution` = quantiles_yule) %>%
  pivot_longer(3:4) -> quantiles_long

ggplot(quantiles_long, aes(x = quantiles_empirical, y = value, color = name)) +
         geom_point() +
         coord_fixed() +
         theme(aspect.ratio = 1) +
  geom_abline(slope = 1) +
  labs(x = "empirical quantiles",
       y = "value (max-normalised)") +
  theme_minimal(base_size = 13) +
  scale_color_brewer(palette = "Set1") 

# tibble(y =  quantiles_simulation, x = quantiles_empirical) %>%
#   
#   # mutate(y = y * sum(x) /
#   #          sum(y)) %>%
#   ggplot(aes(x, y)) +
#   geom_point() +
#   coord_fixed() +
#   theme(aspect.ratio = 1) # +
  # geom_abline(slope = 1, intercept = 0)

#### Histogram for empirical data
# log
tibble(playcount = log(vertex_attr(similar_graph)$playcount)) %>%# View()
  mutate(playcount = ifelse(is.infinite(playcount), 0, playcount)) %>%
ggplot(aes(x = playcount)) +
  geom_histogram() +
  labs(x = "log(playcount)") +
  theme_minimal(base_size = 13) +
  scale_color_brewer(palette = "Set1")

# Standard
ggplot(tibble(playcount = vertex_attr(similar_graph)$playcount), aes(x = playcount)) +
  geom_histogram(bins = 100) +
  theme_minimal(base_size = 13) +
  scale_color_brewer(palette = "Set1")

#### Summary of empirical data (table)
tibble(playcount = vertex_attr(similar_graph)$playcount) %>% 
  summarise(`No. observations` = n(),
            `min.` = min(playcount),
            `25% quantile` = quantile(playcount, 0.25),
            mean = mean(playcount),
            `50% quantile` = quantile(playcount, 0.5),
            `75% quantile` = quantile(playcount, 0.75),
            max = max(playcount),
            sd = sd(playcount)) %>%
  pivot_longer(everything())


#### Stats tests table


ks.test(playcount, simulation_playcount_scaled)
kSamples::ad.test(playcount, simulation_playcount_scaled)


# Yule
ks.test(simulation_playcount_scaled, simulation_yule)
kSamples::ad.test(simulation_playcount_scaled, simulation_yule)




# MLE params, test statistic value and p-value


#### gif of histograms from simulation

# Okazało się mało ciekawe




