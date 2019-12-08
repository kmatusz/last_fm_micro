# install.packages("rvest")
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

api_key <- Sys.getenv("last_fm_token")


path <- "http://ws.audioscrobbler.com/2.0/"

# Similar artist
request <- GET(url = path, 
               query = list(
                 method="artist.getsimilar",
                 artist="cher",
                 api_key=api_key,
                 format="json",
                 limit = 250)
)

response <- content(request, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()


# Tags 
# /2.0/?method=tag.gettopartists&tag=disco&api_key=YOUR_API_KEY&format=json

request <- GET(url = path, 
               query = list(
                 method="tag.gettopartists",
                 tag="polish",
                 api_key=api_key,
                 format="json",
                 limit = 	1000, 
                 page = 1)
)


# jest 17178 artystów, więc 18 pages

response <- content(request, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()

# Właściwe pobieranie ----
artists_list <- vector("list", 10)

# wyśwetla się że jest 17 stron, ale daje się tylko pobrać 10 
for (i in 1:10){
  print(i)
  request <- GET(url = path, 
                 query = list(
                   method="tag.gettopartists",
                   tag="polish",
                   api_key=api_key,
                   format="json",
                   limit = 	1000, 
                   page = i)
  )
  
  
  response <- content(request, as = "text", encoding = "UTF-8")
  
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  
  df %>% mutate(page = i)-> df
  
  artists_list[[i]] <- df
  
  Sys.sleep(1)
  
}


artists_list %>% 
  bind_rows() -> artists

save(artists, file = "data/artists.Rdata")





