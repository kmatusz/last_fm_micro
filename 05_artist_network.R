library(tidyverse)
library(igraph)

load("data/clean_datasets.Rdata")


# test igraph
test_network <- tribble(
  ~artist, ~similar, ~rank,
  "a", "b", 1,
  "a", "c", 2
)

graph <- igraph::graph_from_data_frame(test_network)
graph
plot(graph)

E(graph) # edges
V(graph) # vertices
graph[] # show matrix
edge_attr(graph) # show attributes
vertex_attr(graph)

igraph::random_walk(graph, "a", 2)

similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name)

similar_graph <- igraph::graph_from_data_frame(similar_artists, vertices = general_info)

similar_graph

set.seed(10)
random_artist <- sample(general_info$name, 1)

a <- igraph::random_walk(similar_graph, random_artist, 100, mode = "out", stuck = "return")


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

ggplot(results_df %>% filter(step == 100000), aes(x= playcount)) +
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

similar_graph

set.seed(10)
random_artist <- sample(general_info$name, 1, prob = general_info$playcount)

V(similar_graph)[random_artist]
random_artist

a <- igraph::neighbors(graph = similar_graph, 
                  v = V(similar_graph)[random_artist],
                  mode = "out")

sample(a,1)

# Parameters of execution
no_agents <- 1000
no_steps <- 100
snowball_prob <- 0.5

# setting up variables
agent_ids <- 1:no_agents
last_visited <- rep("", no_agents)
names(last_visited) <- agent_ids
views_count <- vector("list", nrow(general_info))


vertex_attr(similar_graph)$simulation_playcount[which(names(V(similar_graph)) == selected_artist)]

vertex_attr(similar_graph)$simulation_playcount <- rep(0, length(V(similar_graph)))
vertex_attr(similar_graph)$simulation_playcount

# First run (weighted by previous playcount)

selected_artists <- names(sample(V(similar_graph), size = no_agents, replace = TRUE , prob = vertex_attr(similar_graph)$playcount))
selected_artists_id <- which(names(V(similar_graph)) == selected_artists)
last_visited[[i]] <- selected_artist
vertex_attr(similar_graph)$simulation_playcount[selected_artist_id] <- vertex_attr(similar_graph)$simulation_playcount[selected_artist_id] +1


