playcounts_from_selected <-
  function(selected_artists, similar_graph) {
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


sample_random <- function(similar_graph, no_agents, prob = "unif") {
  # probs:
  # Unif - equal weights
  # playcount - weighted by playcount (empirical)
  # simulation - weighted by previous simulations playcount
  
  if (prob == "unif") {
    prob_to_sample <- NULL
  } else if (prob == "playcount") {
    prob_to_sample <- vertex_attr(similar_graph)$playcount
  } else if (prob == "simulation") {
    prob_to_sample <- vertex_attr(similar_graph)$simulation_playcount
  }
  
  names(sample(
    V(similar_graph),
    size = no_agents,
    replace = TRUE,
    prob = prob_to_sample
  ))
  
}

sample_from_graph <- function(last_visited, similar_artists) {
  last_visited %>%
    enframe("id") %>%
    left_join(similar_artists,
              by = c("value" = "artist")) %>%
    group_by(id) %>%
    sample_n(1) %>%
    ungroup() %>%
    .$similar
}

print_cor <- function(similar_graph) {
  tibble(
    playcount = vertex_attr(similar_graph)$playcount,
    simulation_playcount = vertex_attr(similar_graph)$simulation_playcount
  ) %>%
    cor() %>%
    print()
}

init <- function(no_agents,
                 snowball_prob,
                 similar_graph,
                 similar_artists,
                 last_visited = NULL) {
  # setting up variables
  if (is.null(last_visited)) {
    last_visited <- rep(NA, no_agents)
  }
  
  vertex_attr(similar_graph)$simulation_playcount <-
    rep(0, length(V(similar_graph)))
  
  # First run (weighted by previous playcount)
  
  selected_artists <-
    sample_random(similar_graph, no_agents, prob = "unif")
  selected_artists_count <-
    playcounts_from_selected(selected_artists, similar_graph)
  last_visited <- selected_artists
  
  simulation_playcount_last_run <-
    vertex_attr(similar_graph)$simulation_playcount
  vertex_attr(similar_graph)$simulation_playcount <-
    simulation_playcount_last_run + selected_artists_count
  
  
  return(list("similar_graph" = similar_graph, "last_visited" = last_visited))
}

next_runs <- function(no_agents,
                      snowball_prob,
                      similar_graph,
                      similar_artists,
                      last_visited,
                      method = "similar") {
  # Method:
  # similar - select artists similar to previously chosen
  # unif - select artists at random
  
  # artists for snowball
  selected_artists_snowball <-
    sample_random(similar_graph, no_agents, prob = "simulation")
  
  # Use snowball or other approach (similar or unif) for each agent
  use_snowball <- sample(
    c(TRUE, FALSE),
    size = no_agents,
    replace = T,
    prob = c(snowball_prob, 1 - snowball_prob)
  )
  
  
  
  if (method == "unif") {
    selected_artists_alternative <-
      sample_random(similar_graph, no_agents, prob = "unif")
  } else if (method == "similar") {
    # Artist for graph random walk  (similar)
    selected_artists_alternative <-
      sample_from_graph(last_visited, similar_artists)
    
    # Roughly 10% of artists doesn't have similar artist - in this case choose random one
    selected_artists_alternative[is.na(selected_artists_alternative)] <-
      sample_random(similar_graph, 1, prob = "unif")
    
    
  }
  
  # Merge snowball and similar vectors
  selected_artists <-
    ifelse(use_snowball,
           selected_artists_snowball,
           selected_artists_alternative)
  
  # Add new playcounts to graph
  selected_artists_count <-
    playcounts_from_selected(selected_artists, similar_graph)
  
  last_visited <- selected_artists
  
  simulation_playcount_last_run <-
    vertex_attr(similar_graph)$simulation_playcount
  vertex_attr(similar_graph)$simulation_playcount <-
    simulation_playcount_last_run + selected_artists_count
  
  
  
  return(list("similar_graph" = similar_graph, "last_visited" = last_visited))
}

run_simulation <- function(no_agents,
                           no_steps,
                           snowball_prob,
                           similar_graph,
                           similar_artists,
                           method = "similar"){
  init_run <-
    init(no_agents,
         snowball_prob,
         similar_graph,
         similar_artists)
  similar_graph <- init_run$similar_graph
  last_visited <- init_run$last_visited
  
  for (i in 1:no_steps) {
    print(i)
    a <-
      next_runs(no_agents,
                snowball_prob,
                similar_graph,
                similar_artists,
                last_visited,
                method = method)
    similar_graph <- a$similar_graph
    last_visited <- a$last_visited
    # print_cor(similar_graph)
  }
  
  return(list("similar_graph" = similar_graph, "last_visited" = last_visited))
}
