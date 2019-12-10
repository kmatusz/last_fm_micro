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
