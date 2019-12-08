library(tidyverse)
library(jsonlite)

load(file = "data/responses.Rdata")


# Tests of extracting data from 1 artist ----
one_artist <- fromJSON(responses[[1]], flatten = TRUE)

str(one_artist)


one_artist <- one_artist$artist

one_artist[c("image", "url")] <- NULL

one_artist_stripped <-
  one_artist[c("name", "stats", "similar", "tags")]


general <- list()
general$name <- one_artist_stripped$name
general$listeners <- as.numeric(one_artist_stripped$stats$listeners)
general$playcount <- as.numeric(one_artist_stripped$stats$playcount)

general <- as_tibble(general)

similar_raw <- one_artist_stripped$similar$artist

similar_raw %>%
  mutate(artist = one_artist_stripped$name,
         rank = row_number()) %>%
  rename(similar = name) %>%
  select(artist, similar, rank) -> similar

tags_raw <- one_artist_stripped$tags$tag

tags_raw %>%
  select(name) %>%
  mutate(rank = row_number(),
         artist = one_artist_stripped$name) %>%
  rename(tag = name) %>%
  select(artist,
         tag,
         rank) -> tags


# Extracting data from all artists ----
# rank - 1 the highest

# Needed functions
extract_general_info <- function(one_artist) {
  # requires list previously flattened by fromJSON
  one_artist <- one_artist$artist
  
  
  one_artist_stripped <-
    one_artist[c("name", "stats", "similar", "tags")]
  
  
  general <- list()
  general$name <- one_artist_stripped$name
  general$listeners <-
    as.numeric(one_artist_stripped$stats$listeners)
  general$playcount <-
    as.numeric(one_artist_stripped$stats$playcount)
  
  as_tibble(general)
}

extract_similar <- function(one_artist) {
  # browser()
  one_artist <- one_artist$artist
  
  
  if (is.null(one_artist)) {
    return(tibble(
      artist = NA,
      similar = NA,
      rank = NA
    ))
  }
  
  one_artist_stripped <-
    one_artist[c("name", "stats", "similar", "tags")]
  
  similar_raw <- one_artist_stripped$similar$artist
  
  if (is.null(similar_raw) || length(similar_raw) == 0) {
    return(tibble(
      artist = NA,
      similar = NA,
      rank = NA
    ))
  }
  
  similar_raw %>%
    mutate(artist = one_artist_stripped$name,
           rank = row_number()) %>%
    rename(similar = name) %>%
    select(artist, similar, rank) -> similar
  
  similar
}

extract_tags <- function(one_artist) {
  one_artist <- one_artist$artist
  
  if (is.null(one_artist)) {
    return(tibble(
      artist = NA,
      tag = NA,
      rank = NA
    ))
  }
  
  one_artist_stripped <-
    one_artist[c("name", "stats", "similar", "tags")]
  
  
  tags_raw <- one_artist_stripped$tags$tag
  
  tags_raw %>%
    select(name) %>%
    mutate(rank = row_number(),
           artist = one_artist_stripped$name) %>%
    rename(tag = name) %>%
    select(artist,
           tag,
           rank) -> tags
  
  
  tags
}


responses %>%
  map(fromJSON, flatten = TRUE) -> responses_list

responses_list %>%
  map(extract_general_info) %>%
  bind_rows() -> general_info

responses_list %>%
  map(extract_similar) %>%
  bind_rows() %>%
  as_tibble() -> similar_artists

responses_list %>%
  map(extract_tags) %>%
  bind_rows() %>%
  as_tibble() -> tags




save(general_info, similar_artists, tags, file = "data/clean_datasets.Rdata")
