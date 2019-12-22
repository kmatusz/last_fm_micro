# install.packages("degreenet")
library(degreenet)
library(gamlss.dist)
library(tidyverse)

# Yule standard simulation -----
# See: https://www.jstor.org/stable/25651364?seq=1#metadata_info_tab_contents
# For each agent:
# Decide to show random artist
# Or decide to show popular artist 
# snowball_prob - probability of getting into "popular" artist




generate_yule <- function(snowball_prob = 0.5, N_AGENTS = 1000, N_ARTISTS = 1000){
  artists_views <- rep(0, N_ARTISTS)
  
  # First agent
  artist_chosen <- sample.int(N_ARTISTS, size = 1)
  artists_views[artist_chosen] <- artists_views[artist_chosen] + 1
  
  
  for (i in 2:N_AGENTS) {
    use_snowball <-
      sample(c(TRUE, FALSE),
             prob = c(snowball_prob, 1 - snowball_prob),
             size = 1)
    if (use_snowball) {
      # Model snowball effect - probability to choose artist proportional to previous choices
      artist_chosen <-
        sample.int(N_ARTISTS, size = 1, prob = artists_views)
      artist_chosen_list[i] <<- artist_chosen
      
      artists_views[artist_chosen] <-
        artists_views[artist_chosen] + 1
      
    } else{
      # Model random effect
      artist_chosen <- sample.int(N_ARTISTS, size = 1)
      artist_chosen_list[i] <<- artist_chosen
      artists_views[artist_chosen] <-
        artists_views[artist_chosen] + 1
    }
    
  }
  
  artists_views
}


# Manual creation ----
artists_views <- rep(0, N_ARTISTS)

# First agent
artist_chosen <- sample.int(N_ARTISTS, size = 1)
artists_views[artist_chosen] <- artists_views[artist_chosen] + 1


for (i in 2:N_AGENTS) {
  use_snowball <-
    sample(c(TRUE, FALSE),
           prob = c(snowball_prob, 1 - snowball_prob),
           size = 1)
  if (use_snowball) {
    # Model snowball effect - probability to choose artist proportional to previous choices
    artist_chosen <-
      sample.int(N_ARTISTS, size = 1, prob = artists_views)
    artists_views[artist_chosen] <-
      artists_views[artist_chosen] + 1
    
  } else{
    # Model random effect
    artist_chosen <- sample.int(N_ARTISTS, size = 1)
    artists_views[artist_chosen] <-
      artists_views[artist_chosen] + 1
  }
  
}


a <- tibble(artists_views)

a %>% ggplot(aes(artists_views)) +
  geom_histogram(bins = 100)



# Testing yule distribution on last.fm dataset ----

# Load data
temp <- new.env()
load("data/clean_datasets.Rdata", envir = temp)
general_info <- temp$general_info
rm(temp)

# Fast plot
general_info %>% ggplot(aes(playcount)) +
  geom_histogram(bins = 100)

# Estimate yule params using mle
mle_est <- ayulemle(general_info$playcount)
mle_est
ks.test(general_info$playcount, dYULE)

# ks test testing
a <- rnorm(10000)
b <- rnorm(10000)#, mean = 10)
ks.test(a,b)


plot(ecdf(general_info$playcount))
plot(dYULE)
qqplot(general_info$playcount, qYULE(ppoints(100000)))



# Comparison of simulation and empirical ----

snowball_prob <- 0.95
N_AGENTS <- 100000
N_ARTISTS <- 10000

# Params that fit well:
# snowball_prob <- 0.95
# N_AGENTS <- 100000
# N_ARTISTS <- 10000

artist_chosen_list <- vector("numeric", N_AGENTS) # Artist chosen in each run
a <- generate_yule(snowball_prob = snowball_prob,
                N_AGENTS = N_AGENTS,
                N_ARTISTS = N_ARTISTS)

hist(a)



ecdf_comparison <- tibble(
  type = "empirical",
  value = general_info$playcount
)

ecdf_comparison %>%
  rbind(tibble(
    type = "simulation",
    value = a/sum(a)*sum(general_info$playcount))) -> ecdf_comparison


ggplot(ecdf_comparison, aes(x = value, color = type))+
  stat_ecdf() +
  xlim(c(0, 1.0e+06))


ecdf_comparison %>%
  filter(type == "empirical") %>%
  summary()


ecdf_comparison %>%
  filter(type == "simulation") %>%
  summary()

# quartiles don't match 

# Test whether artist chosen at the beginning has higher chances ----

snowball_prob <- 0.7
N_AGENTS <- 100000
N_ARTISTS <- 100

artist_chosen_list <- vector("numeric", N_AGENTS) # Artist chosen in each run
a <- generate_yule(snowball_prob = snowball_prob,
                   N_AGENTS = N_AGENTS,
                   N_ARTISTS = N_ARTISTS)


artist_chosen_list %>% as.character() %>% table() %>%
  as_tibble() %>%
  arrange(-n)
artist_chosen_list
hist(a)

# The bigger the snowball_prob, the more chance there is that most popular artist
# Was chosen as first/second


