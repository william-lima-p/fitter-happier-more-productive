library(echarts4r)

thompson_method <-
  track_df %>%
  distinct(track_name, .keep_all = TRUE) %>%
  left_join(sent_df, by = 'track_name') %>%
  mutate(across(c('pct_sad', 'word_count'), \(x) if_else(is.na(x), 0, x))) %>%
  mutate(lyrical_density = word_count / duration_ms * 1000,
         gloom_index = 1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2,
         gloom_index = gloom_index * 100
         ) %>%
  select(gloom_index, track_name) %>%
  arrange(gloom_index)

my_method <-
  track_df %>%
  distinct(track_name, .keep_all = TRUE) %>%
  left_join(sent_df, by = 'track_name') %>%
  left_join(chord_coefficients %>% select(track_name, Chord_coefficient), by = 'track_name') %>%
  mutate(Chord_coefficient = replace_na(Chord_coefficient, chord_coefficients$Chord_coefficient %>% mean())) %>%
  mutate(Mode_coeficcient = (1 - (-1)**(mode_name == 'minor')) / 16) %>%
  mutate(across(c('pct_sad', 'word_count'), \(x) if_else(is.na(x), 0, x))) %>%
  mutate(lyrical_density = word_count / duration_ms * 1000,
         gloom_index = 1 - ((1 - valence) + (pct_sad * (1 + lyrical_density)) + (1 - Chord_coefficient) + (1 - energy) + Mode_coeficcient) / 5,
         gloom_index = gloom_index * 100
  ) %>%
  select(gloom_index, track_name) %>%
  arrange(gloom_index)

gloom_index_chart <- function(table){
  
  table %>%
    left_join(spotify_df %>% select(track_name, album_name, album_release_date)) %>%
    arrange(desc(album_release_date)) %>%
    group_by(album_name) %>%
    e_chart(album_name) %>%
    e_scatter(gloom_index, symbol_size = 10) %>%
    e_flip_coords() %>%
    e_legend(show = FALSE) %>%
    e_tooltip(trigger = 'item') %>%
    e_theme('roma') %>%
    e_grid(left = 200)
  
}

gloom_index_chart(thompson_method)
gloom_index_chart(my_method)

library(rmarkdown)

rmarkdown::render(input = 'README.Rmd')
