library(rvest)

find_chords <- function(search_url, silent = TRUE){
  
  if(!silent){
    print(search_url)
  }
  
  search <- read_html(search_url)
  
  tab_url <- 
    search %>%
    html_elements("a") %>%
    rvest::html_attr('href') %>%
    .[grepl('/a/wsa/', .)] %>%
    .[4] %>%
    paste0('https://www.songsterr.com',.)
  
  tab <- read_html(tab_url)
  
  chords_url <-
    tab %>%
    html_elements("a") %>%
    rvest::html_attr('href') %>%
    .[grepl('chords',.)] %>%
    .[1] %>%
    paste0('https://www.songsterr.com',.)
  
  chords <- read_html(chords_url)
  
  chords %>%
    html_elements("span") %>%
    html_element("label") %>%
    html_children() %>%
    html_text()
}

get_chord_coefficients <- function(chords){
  chords %>%
    distinct(artist_name, track_name, .keep_all = TRUE) %>%
    unnest(Songsterr_chords) %>%
    filter(!is.na(Songsterr_chords)) %>%
    mutate(Chord_multiplier = case_when(
      grepl("^(?!.*\\bmaj\\b).*[mo]|.*\\bdim\\b.*", Songsterr_chords, perl = T) ~ -1,
      grepl("sus", Songsterr_chords) ~ 0,
      TRUE ~ 1
    )) %>%
    summarize(Total = n(), .by = c('artist_name', 'track_name', 'Chord_multiplier')) %>%
    mutate(Total = Total / sum(Total), .by = c('artist_name', 'track_name')) %>%
    summarize(Chord_coefficient = (sum(Chord_multiplier * Total) + 1) / 2, .by = c('artist_name', 'track_name')) %>%
    arrange(Chord_coefficient)
}

chords <-
  spotify_df %>%
  select(artist_name, track_name) %>%
  mutate(Songsterr_search = paste0(
    'https://www.songsterr.com/?pattern=',
    tolower(artist_name),
    '%20',
    track_name %>%
      tolower() %>%
      utils::URLencode(reserved = TRUE)
  )) %>%
  rowwise() %>%
  mutate(Songsterr_chords = list(tryCatch({find_chords(Songsterr_search, silent = FALSE)}, error = \(e) NA_character_))) %>%
  ungroup() %>%
  select(-Songsterr_search)

saveRDS(chords, 'inst/chords')

chord_coefficients <- get_chord_coefficients(chords)

chords %>%
  left_join(chord_coefficients) %>%
  mutate(Chord_coefficient = replace_na(Chord_coefficient, 0.5)) %>%
  select(track_name, Chord_coefficient)
