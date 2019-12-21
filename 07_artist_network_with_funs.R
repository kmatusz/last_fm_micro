library(tidyverse)
library(igraph)
source('07_funs.R', encoding = "UTF-8")


load("data/clean_datasets.Rdata")





# prepare data and graph ----
similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name)

similar_graph <-
  igraph::graph_from_data_frame(similar_artists, vertices = general_info)


# Snowball with graph ----
# Steps:
# 1. For each agent select initial artist to go to weighted by playcount
# 2. Save obtained distribution
# 3. Same as snowball - with some p go to random artist (weighted by playcount from simulation)
#    With 1-p go to similar artist as before
# For p = 0 only similar



# Parameters of execution
no_agents <- 10000
no_steps <- 50
snowball_prob <- 0.07
method <- "similar"

results <- run_simulation(no_agents, no_steps, snowball_prob, similar_graph, similar_artists, method = method)

a <- list(no_agents = 10000,
          no_steps = 50,
          snowball_prob = 0.07,
          similar_graph, 
          similar_artists,
          method = "similar")
do.call(run_simulation, a)
results <- run_simulation(no_agents, no_steps, snowball_prob, similar_graph, similar_artists, method = method)


similar_graph <- results$similar_graph

params_title <- sprintf("no_agents: %s, no_steps: %s, snowball_prob: %s, method: %s",
                        no_agents,
                        no_steps,
                        snowball_prob,
                        method
                        )

tibble(
  playcount = vertex_attr(similar_graph)$playcount,
  simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
) %>%
  mutate(simulation_playcount = simulation_playcount * max(playcount) /
           max(simulation_playcount)) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, color = name, fill = name)) +
  # geom_density(alpha = 0.5) +
  stat_ecdf() +
  xlim(c(0, 1.0e+06)) +
  labs(title = params_title)




# Results For initialising with random:
# no_agents <- 10000
# no_steps <- 50
# snowball_prob <- 0.05
# Good fit
params_to_test <- list(
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.05,
     similar_graph, 
     similar_artists,
     method = "similar"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.07,
     similar_graph, 
     similar_artists,
     method = "similar"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.1,
     similar_graph, 
     similar_artists,
     method = "similar"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.5,
     similar_graph, 
     similar_artists,
     method = "similar"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.7,
     similar_graph, 
     similar_artists,
     method = "similar"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.9,
     similar_graph, 
     similar_artists,
     method = "similar"),

# Random (unif)
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.05,
     similar_graph, 
     similar_artists,
     method = "unif"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.07,
     similar_graph, 
     similar_artists,
     method = "unif"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.1,
     similar_graph, 
     similar_artists,
     method = "unif"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.5,
     similar_graph, 
     similar_artists,
     method = "unif"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.7,
     similar_graph, 
     similar_artists,
     method = "unif"),
list(no_agents = 10000,
     no_steps = 50,
     snowball_prob = 0.9,
     similar_graph, 
     similar_artists,
     method = "unif")
)

results_list <- vector("list", length(params_to_test))

for (i in 1:length(params_to_test)){
  print(sprintf("Result %s", i))
  results_list[[i]] <- do.call(run_simulation, params_to_test[[i]])[[1]]
}

save(params_to_test, results_list, file = "data/simulation_results.Rdata")


similar_graph <- results_list[[12]]
tibble(
  playcount = vertex_attr(similar_graph)$playcount,
  simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
) %>%
  mutate(simulation_playcount = simulation_playcount * max(playcount) /
           max(simulation_playcount)) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, color = name, fill = name)) +
  stat_ecdf() #+
  # xlim(c(0, 1.0e+06)) +
  # labs(title = params_title)

# Formal assesment
# 1. Plots
# 2. K-S stat