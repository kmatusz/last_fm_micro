# install.packages("rvest")
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

load("data/artists.Rdata")

# Generate your token at last fm site
api_key <- Sys.getenv("last_fm_token")

path <- "http://ws.audioscrobbler.com/2.0/"

# Artist info
request <- GET(url = path, 
               query = list(
                 method="artist.getInfo",
                 artist = artists$topartists.artist.name[1],
                 api_key=api_key,
                 format="json")
)

response <- content(request, as = "text", encoding = "UTF-8")


response %>% jsonlite::fromJSON(flatten = TRUE)

fetching_logs <- tibble(
  artist = artists$topartists.artist.name,
  evaluated = FALSE,
  status_code = NA,
  time = NA
)

responses_list <- vector("list", nrow(fetching_logs))

names(responses_list) <- fetching_logs$artist

time_start <- Sys.time()

for (i in 1:nrow(fetching_logs)){
  
  if (fetching_logs$evaluated[i]){
    next()
  }
  
  request <- GET(url = path, 
                 query = list(
                   method="artist.getInfo",
                   artist = fetching_logs$artist[i],
                   api_key=api_key,
                   format="json")
  )
  
  
  
  response <- content(request, as = "text", encoding = "UTF-8")
  
  responses_list[[i]] <- response
  
  fetching_logs$evaluated[i] <- TRUE
  fetching_logs$status_code[i] <- request$status_code
  fetching_logs$time[i] <- Sys.time()-time_start
  
  if (i %% 10 == 1) {
    print(i)
    print(fetching_logs[i,])
    print(fetching_logs$time[i])
  }
  
  Sys.sleep(1)
  
}

save(fetching_logs, responses_list, file = "data/responses_temp.Rdata")

responses <- responses_list

save(responses, file = "data/responses.Rdata")
