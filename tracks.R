library(httr)

app_id <- "app_spotify"

client_id <- "***"
client_secret <- "***"

spoti_ep <- httr::oauth_endpoint(
  authorize = "https://accounts.spotify.com/authorize",
  access = "https://accounts.spotify.com/api/token")
spoti_app <- httr::oauth_app(app_id, client_id, client_secret)

access_token <- httr::oauth2.0_token(spoti_ep, spoti_app, scope = "user-library-read user-read-recently-played")

library(tidyverse)
library(jsonlite)
get_tracks <- function(offset){
  print(offset)
  api_res <- GET("https://api.spotify.com/v1/me/tracks", config(token = access_token), query = list(limit = 50, offset = offset))
  played <- api_res$content %>%
    rawToChar() %>%
    fromJSON(flatten = TRUE) 
  tibble(track = played$items$track.name %||% NA, 
         trackid = played$items$track.album.id %||% NA, 
         track_uri = gsub("spotify:track:", "", played$items$track.uri) %||% NA,
         album_name = played$items$track.album.name %||% NA, 
         album_id = played$items$track.album.id %||% NA, 
         artist = map_chr(played$items$track.artists, ~.x$name[1] %||% NA), 
         duration = played$items$track.duration_ms %||% NA, 
         explicit = played$items$track.explicit %||% NA, 
         popularity = played$items$track.popularity %||% NA)
}
tracks <- GET("https://api.spotify.com/v1/me/tracks", config(token = access_token), query = list(limit = 50))
content(tracks)$total

colin_tracks <- map_df(seq(0,content(tracks)$total, 50), get_tracks)
dim(colin_tracks)

library(glue)
track_features <- function(id){
  print(id)
  # Évitons de se faire kicker par l'API Spotify
  Sys.sleep(0.1)
  api_res <- GET(glue("https://api.spotify.com/v1/audio-features/{id}"), config(token = access_token))
  res <- api_res$content %>%
    rawToChar() %>%
    fromJSON(flatten = TRUE)
  tibble(danceability = res$danceability %||% NA,
         energy = res$energy %||% NA, 
         key = res$key %||% NA, 
         loudness = res$loudness %||% NA, 
         mode = res$mode %||% NA, 
         speechiness = res$speechiness %||% NA, 
         acousticness = res$acousticness %||% NA, 
         instrumentalness = res$instrumentalness %||% NA, 
         liveness = res$liveness %||% NA, 
         valence = res$valence %||% NA, 
         tempo = res$tempo %||% NA, 
         type = res$type %||% NA, 
         id = res$id %||% NA, 
         uri = res$uri %||% NA, 
         track_href = res$track_href %||% NA, 
         analysis_url = res$analysis_url %||% NA, 
         duration_ms = res$duration_ms %||% NA, 
         time_signature = res$time_signature %||% NA
  )
}
tracks_features <- map_df(colin_tracks$track_uri, track_features)

full_tracks <- full_join(colin_tracks, tracks_features, by = c("track_uri" = "id"))
write_csv(full_tracks, "tracks.csv")
