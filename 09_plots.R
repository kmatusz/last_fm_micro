library(gamlss.dist)
library(tidyverse)
library(igraph)
library(fitdistrplus)

load("data/simulation_results.Rdata")

similar_graph <- results_list[[1]]

#### Fit yule
mle_yule <-
  fitdistrplus::mledist(vertex_attr(similar_graph)$playcount,
                        "YULE",
                        start = list("mu" = 10000))
mle_yule$estimate # 1898.966 - słabo pasuje

#### Quantile-quantile plot

quantiles_simulation <- quantile(vertex_attr(similar_graph)$simulation_playcount, probs = seq(0,0.99,0.01))
quantiles_empirical <- quantile(vertex_attr(similar_graph)$playcount, probs = seq(0,0.99,0.01))
quantiles_yule <- qYULE(seq(0,0.99,0.01), mu = mle_yule$estimate)

tibble(quantiles_empirical,
       quantiles_simulation, 
       quantiles_yule) %>%
  rename(`Quantiles obtained from simulation` = quantiles_simulation,
         `Quantiles obtained from fitted Yule distribution` = quantiles_yule) %>%
  pivot_longer(2:3) -> quantiles_long

ggplot(quantiles_long, aes(x = quantiles_empirical, y = value, color = name)) +
         geom_point() +
         coord_fixed() +
         theme(aspect.ratio = 1) 
tibble(y =  quantiles_simulation, x = quantiles_empirical) %>%
  
  # mutate(y = y * sum(x) /
  #          sum(y)) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  coord_fixed() +
  theme(aspect.ratio = 1) # +
  # geom_abline(slope = 1, intercept = 0)

#### Histogram for empirical data
# log
tibble(playcount = log(vertex_attr(similar_graph)$playcount)) %>%# View()
  mutate(playcount = ifelse(is.infinite(playcount), 0, playcount)) %>%
ggplot(aes(x = playcount)) +
  geom_histogram() +
  labs(x = "log(playcount)")

# Standard
ggplot(tibble(playcount = vertex_attr(similar_graph)$playcount), aes(x = playcount)) +
  geom_histogram(bins = 100)

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

# MLE params, test statistic value and p-value


#### qq plot with empirical vs pareto, yule and simulation

#### gif of histograms from simulation