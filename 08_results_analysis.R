library(gamlss.dist)
library(tidyverse)
library(igraph)
library(fitdistrplus)

load("data/simulation_results.Rdata")


similar_graph <- results_list[[1]]



tibble(
  playcount = sort(vertex_attr(similar_graph)$playcount),
  simulation_playcount = sort(vertex_attr(similar_graph)$simulation_playcount)
) %>%
  mutate(simulation_playcount = simulation_playcount * sum(playcount) /
           sum(simulation_playcount)) -> df_sorted




ggplot(df_sorted, aes(x = playcount, y = simulation_playcount)) +
  geom_point() +
  coord_fixed() +
  theme(aspect.ratio = 1) +
  # stat_smooth() +
  geom_abline(slope = 1, intercept = 0) 




# YULE
mle_yule <-
  fitdistrplus::mledist(vertex_attr(similar_graph)$playcount,
                        "YULE",
                        start = list("mu" = 1000))
mle_yule$estimate # 9878.465 - słabo pasuje
yule_sim <-
  rYULE(length(vertex_attr(similar_graph)$playcount), mu = mle_yule$estimate)


tibble(
  playcount = sort(vertex_attr(similar_graph)$playcount),
  simulation_playcount = sort(yule_sim)
) %>%
  mutate(simulation_playcount = simulation_playcount * sum(playcount) /
           sum(simulation_playcount)) -> df_yule

ggplot(df_yule, aes(x = playcount, y = simulation_playcount)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)


# Stat tests
# Simulation
ks.test(df_sorted$playcount, df_sorted$simulation_playcount)
kSamples::ad.test(df_sorted$playcount, df_sorted$simulation_playcount)


# Yule
ks.test(df_sorted$playcount, yule_sim)
kSamples::ad.test(df_sorted$playcount, yule_sim)

# In all cases H0 refused

# Compare yule and simulation
df_sorted %>%
  mutate(type = "simulation") %>%
  rbind(df_yule %>%
          mutate(type = "Yule distribution")) %>%
  ggplot(aes(x = playcount, y = simulation_playcount, color = type)) +
  geom_point() +
  coord_fixed() +
  theme(aspect.ratio = 1) +
  geom_abline(slope = 1, intercept = 0)


mle_yule <-
  fitdistrplus::mledist(vertex_attr(similar_graph)$playcount,
                        "YULE",
                        start = list("mu" = 1000))



# QQ plot proper
quanitles_simulation <- quantile(vertex_attr(similar_graph)$simulation_playcount, probs = seq(0,0.99,0.01))
quanitles_empirical <- quantile(vertex_attr(similar_graph)$playcount, probs = seq(0,0.99,0.01))
quanitles_yule <- qYULE(seq(0,0.99,0.01), mu = mle_yule$estimate)


tibble(y =  quanitles_yule, x = quanitles_empirical) %>%
  
  mutate(y = y * sum(x) /
           sum(y)) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  coord_fixed() +
  theme(aspect.ratio = 1) +
  geom_abline(slope = 1, intercept = 0)


#######
# tibble(
#   playcount = vertex_attr(similar_graph)$playcount,
#   simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
# ) -> df
# df %>%
#   mutate(simulation_playcount = simulation_playcount * max(playcount) /
#            max(simulation_playcount)) %>%
#   pivot_longer(1:2) %>%
#   ggplot(aes(value, color = name, fill = name)) +
#   stat_ecdf()