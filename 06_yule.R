# Yule standard simulation -----
# See: https://www.jstor.org/stable/25651364?seq=1#metadata_info_tab_contents
# For each agent:
# Decide to show random artist
# Or decide to show popular artist 
# snowball_prob - probability of getting into "popular" artist


snowball_prob <- 0.3
N_AGENTS <- 10000
N_ARTISTS <- 1000

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



# My idea about yule ----
# 



