# please read the article
# https://www.thompsonanalytics.com/blog/fitter-happier/

library(spotifyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(purrr)
library(rvest)
library(stringr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR KEY')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR KEY')

spotify_df <- get_artist_audio_features('radiohead') %>% as_tibble()

non_studio_albums <- c('TKOL RMX 1234567', 'In Rainbows Disk 2', 'Com Lag: 2+2=5', 'I Might Be Wrong', 'OK Computer OKNOTOK 1997 2017', 'KID A MNESIA')
spotify_df <- filter(spotify_df, !album_name %in% non_studio_albums)

saveRDS(spotify_df, 'inst/spotify_df')

# genius

token <- 'YOUR KEY'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q='
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('radiohead')
genius_artists

baseURL <- 'https://api.genius.com/artists/'
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

summary(track_lyric_urls[[1]])

lyric_scraper <- function(url) {
  # read_html(url) %>%
  #   # html_node('lyrics') %>%
  #   html_nodes(xpath = '//*[@id="lyrics-root"]') %>%
  #   html_text
  
  read_html(url) %>%
    html_nodes(xpath = '//*[@id="lyrics-root"]') %>%
    html_children() %>%
    .[2 ]%>%
    gsub(pattern = '<.*?>', replacement = " ", )
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  
  print(x)
  
  # add in error handling
  lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
  if (class(lyrics) != 'try-error') {
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- tolower(str_trim(lyrics))
  } else {
    lyrics <- NA
  }
  
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  
  return(tots)
})

genius_df %>% filter(!is.na(lyrics))

saveRDS(genius_df, 'inst/genius_df')

genius_df$track_name[genius_df$track_name == 'Packt Like Sardines in a Crushd Tin Box'] <- 'Packt Like Sardines in a Crushed Tin Box'
genius_df$track_name[genius_df$track_name == 'Weird Fishes / Arpeggi'] <- 'Weird Fishes/ Arpeggi'
genius_df$track_name[genius_df$track_name == 'A Punchup at a Wedding'] <- 'A Punch Up at a Wedding'
genius_df$track_name[genius_df$track_name == 'Dollars and Cents'] <- 'Dollars & Cents'
genius_df$track_name[genius_df$track_name == 'Bullet Proof...I Wish I Was'] <- 'Bullet Proof ... I Wish I was'

genius_df <- genius_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  filter(!duplicated(track_name_join)) %>%
  select(-track_name)

track_df <- spotify_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  left_join(genius_df, by = 'track_name_join') %>%
  select(any_of(c('track_name', 'valence', 'energy', 'mode_name', 'duration_ms', 'lyrics', 'album_name', 'album_release_year', 'album_img')))

saveRDS(track_df, 'inst/track_df')

# quantifying sentiment

track_df %>%
  select(valence, track_name) %>%
  arrange(valence) %>%
  slice(1:10)

library(tidytext)

sad_words <-
  # sentiments %>%
  get_sentiments('nrc') %>%
  # filter(lexicon == 'nrc', sentiment == 'sadness') %>%
  filter(sentiment == 'sadness') %>%
  select(word) %>%
  mutate(sad = T)

sent_df <- track_df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(sad_words, by = 'word') %>%
  group_by(track_name) %>%
  summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
            word_count = n()) %>%
  ungroup

saveRDS(sent_df, 'inst/sent_df')

sent_df %>%
  select(pct_sad, track_name) %>%
  arrange(-pct_sad) %>%
  head(10)