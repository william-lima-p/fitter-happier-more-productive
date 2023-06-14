In 2017, thompsonanalytics.com released an
<a href="https://www.thompsonanalytics.com/blog/fitter-happier/">article</a>
calculating an index representing the sadness of a song, in order to
determine Radiohead’s saddest song.

At that time, I was not in the field of programming yet. Recently, I
read the article again by chance, and realized it was programmed in R.
So I decided to reproduce the article code and add up a little twist.

The Spotify API provides good information about the songs. Particularly,
the author used the feature **valence**, but I decided to include
**energy**, and also the **mode_name** (minor or major). A song in minor
tends to be more sad – although it is not the only factor – so the
inclusion seemed natural to me.

Further, I decided to use web scraping into
<a href="https://www.songsterr.com/">Songsterr</a> in order to find out
the percentage of the minor chords among all of the song chords. Add
diminshed chords to that. Songsterr’s chord section is not as
trustworthy as the tab section, but it was easier to web scrape and come
up with insights.

# Thompson part

You can expect the values to be nowadays different from the article.
That is because Spotify’s API returned different `valence` scores back
in 2017. Also, you might expect some changes in Genius’s API as well,
although more rarely.

``` r
spotify_df %>% arrange(valence) %>% select(track_name, valence) %>% head(10)
```

    ## # A tibble: 10 × 2
    ##    track_name                                                      valence
    ##    <chr>                                                             <dbl>
    ##  1 We Suck Young Blood                                              0.0378
    ##  2 True Love Waits                                                  0.0381
    ##  3 MK 2                                                             0.039 
    ##  4 MK 1                                                             0.0397
    ##  5 The Tourist                                                      0.0398
    ##  6 Motion Picture Soundtrack                                        0.0427
    ##  7 Go Slowly                                                        0.0446
    ##  8 Life In a Glasshouse                                             0.0466
    ##  9 Videotape                                                        0.0506
    ## 10 Tinker Tailor Soldier Sailor Rich Man Poor Man Beggar Man Thief  0.0508

I have reproduced the article steps, except by the function
`lyric_scraper`, which had to be adjusted since the Genius (lyrics
website) html structure had changed since 2017.

``` r
lyric_scraper <- function(url) {
  # read_html(url) %>%
  #   html_node('lyrics') %>%
  #   html_text
  
  read_html(url) %>%
    html_nodes(xpath = '//*[@id="lyrics-root"]') %>%
    html_children() %>%
    .[2] %>%
    gsub(pattern = '<.*?>', replacement = " ", )
}
```

I’m also not a fan of rescaling the `gloom_index`, because it’s a method
influenced entirely by the data you currently have (minimum and maximum
turn into 0 and 100), making it difficult to compare with other
analyses. So I just multiplied the `gloom_index` by 100 instead, to get
a value between 0 and 100.

``` r
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

head(thompson_method, 20)
```

    ## # A tibble: 20 × 2
    ##    gloom_index track_name                                                     
    ##          <dbl> <chr>                                                          
    ##  1        23.8 Give Up The Ghost                                              
    ##  2        37.2 True Love Waits                                                
    ##  3        42.8 Motion Picture Soundtrack                                      
    ##  4        46.3 Tinker Tailor Soldier Sailor Rich Man Poor Man Beggar Man Thief
    ##  5        46.5 We Suck Young Blood                                            
    ##  6        47.9 Pyramid Song                                                   
    ##  7        48.4 Life In a Glasshouse                                           
    ##  8        48.5 Let Down                                                       
    ##  9        49.0 Videotape                                                      
    ## 10        50.1 The Numbers                                                    
    ## 11        50.2 The Tourist                                                    
    ## 12        50.5 Codex                                                          
    ## 13        50.5 Exit Music (For A Film)                                        
    ## 14        50.6 Planet Telex                                                   
    ## 15        51.0 The Gloaming                                                   
    ## 16        51.2 Creep                                                          
    ## 17        51.3 Down Is The New Up                                             
    ## 18        52.0 MK 2                                                           
    ## 19        52.0 MK 1                                                           
    ## 20        52.2 Go Slowly

# My twists

Let’s start with the web scrapping into Songsterr.

``` r
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

chords %>%
  unnest(Songsterr_chords) %>%
  summarize(glue::glue_collapse(Songsterr_chords, sep = ','), .by = 'track_name')
```

    ## # A tibble: 120 × 2
    ##    track_name                    `glue::glue_collapse(Songsterr_chords, sep = ",")`                       
    ##    <chr>                         <glue>                                                                   
    ##  1 Everything In Its Right Place <NA>                                                                     
    ##  2 Kid A                         C,C♯maj13,Cm,D♯,F,C,C♯maj13,Cm,D♯,F,C,C♯maj13,Cm,D♯,F,C,C♯maj13,Cm,D♯,F,…
    ##  3 The National Anthem           C♯,D,D♯,E,F,F♯,G,G♯,A,A♯,C,C♯,D,D♯,E,F,F♯,G,G♯,A,A♯,C,C♯,D,D♯,E,F,F♯,G,G…
    ##  4 How to Disappear Completely   <NA>                                                                     
    ##  5 Treefingers                   <NA>                                                                     
    ##  6 Optimistic                    Am/D,Bm,D,Dadd9,A♯6,D,C/D,Am/D,Bm,D,Dadd9,A♯6,D,C/D,A♯6,D,C/D,Dm,C/D,Em/…
    ##  7 In Limbo                      <NA>                                                                     
    ##  8 Idioteque                     Cadd2,B,Em,Gm,D♯,C,Cadd2,B,Em,Gm,D♯,C,Cadd2,B,Em,Gm,D♯,C,Cadd2,B,Em,Gm,D…
    ##  9 Morning Bell                  Am,A7M,G,D,Em,G♯m,Am,A7M,Am,A7M,Am,A7M,G,D,G,D,Am,A7M,Am,A7M,Am,A7M,G,D,…
    ## 10 Motion Picture Soundtrack     C♯,E,G,Csus2,Bmadd9,Csus2,G,Csus2,Bmadd9,Csus2,Em,C,G,G,Csus2,Bmadd9,Csu…
    ## # … with 110 more rows

Some songs don’t have chords registered in Songsterr’s chord section, so
they get `NA` values. I complete these values later using the global
`mean`.

Now, I calculate the percentage of three categories of chords:

- Category 1: minor and diminished chords

- Category 2: suspended chords

- Category 3: other chords

Suspended chords are ambiguous, in the sense that they are neither sad
nor happy. It depends entirely on which chord they resolve into. So they
are separated into their own category.

Then, we compute the coefficient *Category 3* minus *Category 1*, which
is between -1 and 1, and then embed the image into the interval $[0,1]$.

``` r
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

chord_coefficients <- get_chord_coefficients(chords)
head(chord_coefficients, 10)
```

    ## # A tibble: 20 × 3
    ##    artist_name track_name                Chord_coefficient
    ##    <chr>       <chr>                                 <dbl>
    ##  1 Radiohead   Lotus Flower                          0.318
    ##  2 Radiohead   Weird Fishes/ Arpeggi                 0.368
    ##  3 Radiohead   Where I End and You Begin             0.368
    ##  4 Radiohead   Street Spirit (Fade Out)              0.406
    ##  5 Radiohead   Knives Out                            0.411
    ##  6 Radiohead   Climbing Up the Walls                 0.413
    ##  7 Radiohead   Lucky                                 0.417
    ##  8 Radiohead   15 Step                               0.5  
    ##  9 Radiohead   Optimistic                            0.513
    ## 10 Radiohead   Kid A                                 0.534
    ## 11 Radiohead   Go Slowly                             0.541
    ## 12 Radiohead   Down Is The New Up                    0.545
    ## 13 Radiohead   My Iron Lung                          0.557
    ## 14 Radiohead   Nude                                  0.565
    ## 15 Radiohead   No Surprises                          0.587
    ## 16 Radiohead   Motion Picture Soundtrack             0.6  
    ## 17 Radiohead   Morning Bell                          0.608
    ## 18 Radiohead   We Suck Young Blood                   0.611
    ## 19 Radiohead   There, There                          0.615
    ## 20 Radiohead   Paranoid Android                      0.635

Now, the final calculation. As I mentioned, we can add **energy** and
**Chord_coefficient** into the calculation. The feature **mode_name**
was added so that the `gloom_index` was 1/16 sadder when the song was in
minor.

``` r
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

head(my_method, 20)
```

    ## # A tibble: 20 × 2
    ##    gloom_index track_name               
    ##          <dbl> <chr>                    
    ##  1        49.1 Give Up The Ghost        
    ##  2        51.7 Motion Picture Soundtrack
    ##  3        52.7 True Love Waits          
    ##  4        53.1 We Suck Young Blood      
    ##  5        54.5 Codex                    
    ##  6        54.5 MK 1                     
    ##  7        56.4 Exit Music (For A Film)  
    ##  8        56.5 Lucky                    
    ##  9        56.8 Street Spirit (Fade Out) 
    ## 10        57.1 Go Slowly                
    ## 11        57.4 I Will                   
    ## 12        57.8 Glass Eyes               
    ## 13        57.8 MK 2                     
    ## 14        58.4 Treefingers              
    ## 15        58.4 Daydreaming              
    ## 16        58.8 Sail To The Moon         
    ## 17        59.1 Videotape                
    ## 18        59.1 Life In a Glasshouse     
    ## 19        59.2 Nude                     
    ## 20        60.3 Untitled

The weirdness found in the former method, like *Tinker Tailor Soldier
Sailor Rich Man Poor Man Beggar Man Thief*, *Planet Telex* and *The
Gloaming* with low values, has been adjusted! Now, our top 20 appears to
only have sad songs, indeed.

``` r
gloom_index_chart <- function(table, title = ''){
  
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
    e_grid(left = 200) %>%
    e_title(title)
  
}

gloom_index_chart(thompson_method, 'Thompson method')
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
gloom_index_chart(my_method, 'My method')
```

![](README_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->
