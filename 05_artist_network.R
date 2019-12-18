library(tidyverse)
library(igraph)

load("data/clean_datasets.Rdata")

# prepare data and graph ----
similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name)

similar_graph <-
  igraph::graph_from_data_frame(similar_artists, vertices = general_info)

similar_graph

# test igraph ----
test_network <- tribble(~ artist, ~ similar, ~ rank,
                        "a", "b", 1,
                        "a", "c", 2)

graph <- igraph::graph_from_data_frame(test_network)
graph
plot(graph)

E(graph) # edges
V(graph) # vertices
graph[] # show matrix
edge_attr(graph) # show attributes
vertex_attr(graph)

igraph::random_walk(graph, "a", 2)


# First random walk ----
set.seed(10)
random_artist <- sample(general_info$name, 1)

a <-
  igraph::random_walk(similar_graph,
                      random_artist,
                      100,
                      mode = "out",
                      stuck = "return")


no_agents <- 100
no_steps <- 1000000

results_list <- vector("list", no_agents)

for (i in 1:no_agents) {
  random_artist <- sample(general_info$name, 1)
  
  walk <- igraph::random_walk(similar_graph,
                              random_artist,
                              no_steps,
                              mode = "out",
                              stuck = "return")
  results_list[[i]] <- tibble(name = names(walk)) %>%
    mutate(step = row_number(),
           agent = i)
}

results_list %>%
  bind_rows() %>%
  left_join(general_info) %>%
  select(-listeners) -> results_df

ggplot(results_df %>% filter(step == 100000), aes(x = playcount)) +
  geom_histogram()

# For:
# no_agents <- 100
# no_steps <- 1000000
# We get massive playcounts for superstars

# For inverted it is evolving into right pattern very slowly
# nothing visible happens
# no_agents <- 100000
# no_steps <- 1000

results_df %>%
  group_by(step) %>%
  summarise(median_playcount = median(playcount, na.rm = T)) %>%
  plot()


# histogram after filtering 0
general_info %>%
  # filter(playcount >0)
  ggplot(aes(playcount)) +
  stat_ecdf()


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
snowball_prob <- 0.05

# setting up variables
last_visited <- rep("", no_agents)

vertex_attr(similar_graph)$simulation_playcount <-
  rep(0, length(V(similar_graph)))

# First run (weighted by previous playcount)

selected_artists <- names(sample(
  V(similar_graph),
  size = no_agents,
  replace = TRUE #,
  #prob = vertex_attr(similar_graph)$playcount
))
# selected_artists_count <-
#   names(V(similar_graph)) %in% selected_artists


playcounts_from_selected <- function(selected_artists, similar_graph){
  selected_artists %>% 
    table() %>% 
    as_tibble() %>% 
    rename(artist = ".") -> selected_artists_freq
  
  
  enframe(names(V(similar_graph))) %>% 
    left_join(selected_artists_freq, 
              by = c("value" = "artist")) %>%
    replace_na(list("n" = 0)) %>%
    .$n
}

selected_artists_count <- playcounts_from_selected(selected_artists, similar_graph) 
# selected_artists %>% 
#   table() %>% 
#   as_tibble() %>% 
#   rename(artist = ".") -> selected_artists_freq
# 
# 
# enframe(names(V(similar_graph))) %>% 
#   left_join(selected_artists_freq, 
#             by = c("value" = "artist")) %>%
#   replace_na(list("n" = 0)) %>%
#   .$n -> selected_artists_count

last_visited <- selected_artists

simulation_playcount_last_run <-
  vertex_attr(similar_graph)$simulation_playcount
vertex_attr(similar_graph)$simulation_playcount <-
  simulation_playcount_last_run + selected_artists_count

# vertex_attr(similar_graph)$simulation_playcount
# last_visited


# Next runs (weighted by simulation playcount)




# Main snowball ----
for (i in 1:no_steps) {
  # if(i %% 10 == 1) 
    print(i)
  # artists for snowball
  selected_artists_snowball <- names(
    sample(
      V(similar_graph),
      size = no_agents,
      replace = TRUE,
      prob = vertex_attr(similar_graph)$simulation_playcount
    )
  )
  
  # Artist for graph random walk  (similar)
  last_visited %>%
    enframe("id") %>%
    left_join(similar_artists,
              by = c("value" = "artist")) %>%
    group_by(id) %>%
    sample_n(1) %>%
    ungroup() %>%
    .$similar -> sampled_from_graph
  
  selected_artists_similar <- sampled_from_graph
  
  # Use snowball or similar approach for each agent
  use_similar <- sample(
    c(TRUE, FALSE),
    size = no_agents,
    replace = T,
    prob = c(1 - snowball_prob, snowball_prob)
  )
  
  
  
  # Roughly 10% of artists doesn't have similar artist - in this case choose random one
  selected_artists_similar[is.na(selected_artists_similar)] <- names(
    sample(
      V(similar_graph),
      size = 1,
      replace = TRUE
    )
  )
  
  # use_similar <-
  #   ifelse(is.na(selected_artists_similar), FALSE, use_similar)
    
  # Merge snowball and similar vectors
  selected_artists <-
    ifelse(use_similar,
           selected_artists_similar,
           selected_artists_snowball)
  
  # Add new playcounts to graph
  selected_artists %>% 
    table() %>% 
    as_tibble() %>% 
    rename(artist = ".") -> selected_artists_freq
  
  
  enframe(names(V(similar_graph))) %>% 
    left_join(selected_artists_freq, 
              by = c("value" = "artist")) %>%
    replace_na(list("n" = 0)) %>%
    .$n -> selected_artists_count
  
  
  last_visited <- selected_artists
  
  simulation_playcount_last_run <-
    vertex_attr(similar_graph)$simulation_playcount
  vertex_attr(similar_graph)$simulation_playcount <-
    simulation_playcount_last_run + selected_artists_count
  
  tibble(
    playcount = vertex_attr(similar_graph)$playcount,
    simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
  ) %>% 
    cor() %>%
    print()
  
}

tibble(
  playcount = vertex_attr(similar_graph)$playcount,
  simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
) %>% #cor()
  ggplot(aes(playcount, simulation_playcount)) +
  geom_point()

# vertex_attr(similar_graph)

tibble(
  playcount = vertex_attr(similar_graph)$playcount,
  simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
) %>% 
  mutate(simulation_playcount = simulation_playcount*max(playcount)/max(simulation_playcount)) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(value, color = name, fill = name)) +
  # geom_density(alpha = 0.5) +
  # xlim(c(0, 1.0e+04))
  stat_ecdf()





# Results For initialising with random:
# no_agents <- 10000
# no_steps <- 50
# snowball_prob <- 0.05
# Good fit




# 50 steps enough to converge  
# For just 0.05 prob of choosing popular we get results close to original data (without inputing this information)


# After 10 steps





