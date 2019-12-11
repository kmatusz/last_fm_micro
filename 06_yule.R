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


snowball_prob <- 0.8
N_AGENTS <- 10000
N_ARTISTS <- 1000

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
      artists_views[artist_chosen] <-
        artists_views[artist_chosen] + 1
    }
    
  }
  
  artists_views
}



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
temp <- new.env()
load("data/clean_datasets.Rdata", envir = temp)
general_info <- temp$general_info
rm(temp)

general_info %>% ggplot(aes(playcount)) +
  geom_histogram(bins = 100)




mle_est <- ayulemle(general_info$playcount)
mle_est
ks.test(general_info$playcount, dYULE)


a <- rnorm(10000)
b <- rnorm(10000)#, mean = 10)
ks.test(a,b)


plot(ecdf(general_info$playcount))
plot(dYULE)

qqplot(general_info$playcount, qYULE(ppoints(100000)))



# Comparison of simulation and empirical

a <- b
artist_chosen_list <- vector("numeric", N_AGENTS)
a <- generate_yule(snowball_prob = 0.8, N_AGENTS = 100000, N_ARTISTS = 9998)

t <- tibble(a, idx = 1:9998)
t %>%
  arrange(desc(a)) %>%
  head(10) %>%
  .$idx -> top_artists



ecdf_comparison <- tibble(
  type = "empirical",
  value = general_info$playcount
)

ecdf_comparison %>%
  rbind(tibble(
    type = "simulation",
    value = a/sum(a)*sum(general_info$playcount))) -> ecdf_comparison


ggplot(ecdf_comparison, aes(x = value, color = type))+
  stat_ecdf()# +
  xlim(c(0, 1.0e+06))


ecdf_comparison %>%
  filter(type == "empirical") %>%
  summary()


ecdf_comparison %>%
  filter(type == "simulation") %>%
  summary()

# quartiles don't match 


