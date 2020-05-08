library(gamlss.dist)
library(tidyverse)
library(igraph)
library(fitdistrplus)

load("data/simulation_results.Rdata")


similar_graph <- results_list[[1]]



load("data/clean_datasets.Rdata")

# prepare data and graph ----
similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name)


tags %>% View()

# TODO:
# Spojrzeć na wykres jak USL - pokolorować po rankingu playcountu ok
# Jakieś miary dla sieci - coś co mówi o beetwenness dla danego noda ok
# I porównać te miary z otrzymanym playcountem ok

# Zrobić klastrowanie DBSCAN/kmeans z więcej klastrów



