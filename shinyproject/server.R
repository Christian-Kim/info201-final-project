library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)

files <- dir("./data")
music_data <- do.call(rbind,lapply(paste0("./data/", files), read.csv, stringsAsFactors = FALSE))
music_data <- mutate(music_data, popularity = popularity / 100.0,
               genre = music_data[,1],
               songBy = paste(track_name, "by", artist_name))

distinct_songs <- distinct(music_data, track_name, .keep_all = TRUE)

genre_list <- as.vector(distinct(music_data, genre)[,1])

radar_chart <- function(group_names, df, columns, W, H, M) {
  if (length(group_names) == 0 || length(columns) == 0 || nrow(df) == 0) {
    return (plot_ly(type = 'scatterpolar', mode = 'marker', fill = 'toself') %>%
              layout(
                p,
                polar = list(
                  radialaxis = list(
                    visible = T,
                    range = c(0, 100)
                  )
                ),
                paper_bgcolor = 'transparent')
            )
  }
  p <- plot_ly(type = 'scatterpolar', mode = 'marker', fill = 'toself')
  for (i in seq(length(group_names))) {
    p <- add_trace(p, r = as.numeric(df[i,]), theta = columns, name = group_names[i])
  }
  p <- layout(
    p,
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0, 100)
      )
    ),
    paper_bgcolor = 'transparent',
    autosize = F,
    width = W,
    height = H,
    margin = M
  )
  return (p)
}

get_genre_stats <- function(genres, input_df, columns) {
  df <- data.frame(matrix(ncol = length(columns), nrow = length(genres)))
  colnames(df) <- columns
  if (length(genres) == 0) {
    return (radar_chart(c(), df, columns))
  }
  for (i in seq(length(genres))) {
    for (j in seq(length(columns))) {
      col_sym <- rlang::sym(columns[j])
      genre_df <- filter(music_data, genre == genres[i])
      summarized_col <- summarize(genre_df, mean(!!col_sym))
      df[i, j] <- summarized_col[1,1] * 100
    }
  }
  return (radar_chart(genres, df, columns))
}

playlist <- music_data %>% head(1) %>% filter(artist_name == "")
write.csv(playlist, "./playlist.csv", row.names = FALSE)

my_server <- function(input, output) {
  output$genre_list <- renderUI({
    checkboxGroupInput(
      "genres",
      "Select Genres",
      genre_list,
      selected = c("Pop", "Comedy")
    )
  })
  
  output$radarchart_genres <- renderPlotly({
    columns <- input$attributes
    genres <- input$genres
    df <- data.frame(matrix(ncol = length(columns), nrow = length(genres)))
    colnames(df) <- columns
    if (length(genres) == 0) {
      return (radar_chart(c(), df, columns))
    }
    for (i in seq(length(genres))) {
      for (j in seq(length(columns))) {
        col_sym <- rlang::sym(columns[j])
        genre_df <- filter(music_data, genre == genres[i])
        summarized_col <- summarize(genre_df, mean(!!col_sym))
        df[i, j] <- summarized_col[1,1] * 100
      }
    }
    radar_margins = list(l = 150)
    return (radar_chart(genres, df, columns, W = 1000, H = 600, M = radar_margins))
  })
  
  output$genre_selection <- renderUI({
    selectizeInput(
      "genre_selection",
      "Select a genre to add songs from",
      choices = genre_list,
      selected = "Pop"
    )
  })

  observeEvent(
    input$update_playlist,
    {output$playlist <- renderTable({
      print("add")
      playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
      selected_song <- isolate(input$playlist_select)
      playlist_additions <- filter(music_data, songBy == selected_song)
      playlist <- rbind(playlist, playlist_additions)
      write.csv(playlist, "./playlist.csv", row.names = FALSE)
      songByOnly <- distinct(playlist, songBy) %>% select("Your Playlist" = songBy)
      return (songByOnly)
    })
  })
  
  observeEvent(
    input$clear_playlist,
    {output$playlist <- renderTable({
      print("remove")
      playlist <- music_data %>% head(1) %>% filter(artist_name == "")
      write.csv(playlist, "./playlist.csv", row.names = FALSE)
      songByOnly <- select(playlist, "Your Playlist" = songBy)
      return (songByOnly)
    })
  })
  
  output$song_selection <- renderUI({
    songs <- as.vector(filter(music_data, genre == input$genre_selection) %>% select(songBy))
    selectizeInput(
      'playlist_select',
      'Add to your playlist',
      choices = songs,
      multiple = FALSE,
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist' = FALSE
      )
    )
  })
  
  observeEvent(
    c(input$update_playlist, input$clear_playlist),
    {output$radarchart_playlist <- renderPlotly({
      playlist_df <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
      columns <- input$attributes_playlist
      summarized_df <- data.frame(matrix(ncol = length(columns), nrow = 1))
      colnames(summarized_df) <- columns
      for (j in seq(length(columns))) {
        col_sym <- rlang::sym(columns[j])
        summarized_col <- summarize(playlist_df, mean(!!col_sym))
        summarized_df[1, j] <- summarized_col[1,1] * 100
      }
      
      radar_margin = list(l = 50, r = 50, b = 100, t = 0, margin = 2)
      return (radar_chart(c("Your Playlist's Features:"), summarized_df, columns, W = 500, H = 500, M = radar_margin))
    })
  })
  

  output$radarchart_selected_song <- renderPlotly({
    playlist_df <- filter(music_data, songBy %in% input$playlist_select)
    columns <- input$attributes_playlist
    summarized_df <- data.frame(matrix(ncol = length(columns), nrow = 1))
    colnames(summarized_df) <- columns
    for (j in seq(length(columns))) {
      col_sym <- rlang::sym(columns[j])
      summarized_col <- summarize(playlist_df, mean(!!col_sym))
      summarized_df[1, j] <- summarized_col[1,1] * 100
    }
    
    radar_margin = list(l = 50, r = 50, b = 50, t = 0, margin = 3)
    return (radar_chart(c("Selected Song's Features:"), summarized_df, columns, W = 600, H = 600, M = radar_margin))
  })
  
  output$radarchart <- renderPlotly({
    return (get_genre_stats(input$genres, music_data, input$attributes))
  })
  
  output$outputPlot <- renderPlot({
    test1 <- filter(music_data, genre == toString(input$list_of_genres_plot))
    plot <- ggplot(test1, aes(x = get(input$x), y = popularity)) +
              geom_smooth() +
              labs(x = input$x, y = "popularity", title = paste(input$x, "vs popularity for", input$list_of_genres_plot, "Music")) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot)
  })
  
  output$genres_drop_down <- renderUI({
    selectInput("list_of_genres_plot", "Pick a Genre", genre_list)
  })
  
  observeEvent(
    input$update_recs,
    {output$song_recommendation_table <- DT::renderDataTable({
      playlist_recommendation <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
      if (nrow(playlist_recommendation) == 0) {
        DT::datatable(playlist_recommendation, options = list(searching = FALSE))
      }
      playlist_songBy <- select(playlist_recommendation, songBy)
      playlist_averages <- filter(distinct_songs, songBy %in% playlist_songBy$songBy) %>%
        summarize(popularity = mean(popularity),
                  acousticness = mean(acousticness),
                  danceability = mean(danceability),
                  energy = mean(energy),
                  instrumentalness = mean(instrumentalness), 
                  liveness = mean(liveness), 
                  speechiness = mean(speechiness), 
                  valence = mean(valence))
      
      distinct_songs_percent_diff <- distinct_songs %>% mutate(percent_diff =
                                                                 (abs(acousticness - playlist_averages[1,2])) + 
                                                                 (abs(danceability - playlist_averages[1,3])) +
                                                                 (abs(energy - playlist_averages[1,4])) +
                                                                 (abs(liveness - playlist_averages[1,5])) +
                                                                 (abs(speechiness - playlist_averages[1,6])) +
                                                                 (abs(valence - playlist_averages[1,7]))) %>% 
        arrange(percent_diff) %>% filter(percent_diff != 0) %>% select(Reccomendations = songBy)
      DT::datatable(distinct_songs_percent_diff, options = list(searching = FALSE))
    })
  })
  output$header1 <- renderText({
    return("Here the user is presented with checkboxes to select genres and attributes. Once specific genres and attributes have been selected the data is taken and formatted into a radar chart which will display the levels of each attribute and categorize each genre by color.
This layout grants an efficient way to view many different attributes of many different genres. It allows a user to analyze the overall attributes of genres relative to other genres.  
")
  })
  output$header2 <- renderText({
    return("Here the user can select between two different subtabs. The first subtab Playlist Creation allows the user search for specific tracks within a specific genre. Once a user has selected a track the Music Analyzer present that individual track's radar chart containing the levels for attributes that can be selected to the left of the radar chart. If the user likes the track they can push the Add this song to your playlist button to add the song into your playlist. All the songs in the user's playlist are displayed in a sidebar panel with a radar chart displaying the average attribute data from all the songs in the playlist. Once the user is satisfied with their playlist they can then go to the Song Recommendation subtab and press the Press to see songs similar to the ones in your playlist button to see the tracks from the data with the most similar attribute levels. These features can be used to assist the user by introducing tracks that they are likely to enjoy.   
")
  })
  output$header_playlist_creation <- renderText({
    return("Here the user can choose a genre and an attribute to plot. Once the genre and attributes have been selected, the plot will filter and select the corresponding tracks from the data and will plot the data according to the popularity. This can be used to analyze what attributes of specific genres result in a popular/successful track.
")
  })
  output$song_recommendation_text_output <- renderText({
    return("The design team 40 Plus 1 would like to introduce our newest app, Music Analyzer. Running as a potential extension of Spotify, Music Analyzer uses data from kaggle user tomigelo's Spotify Audio Features which contains audio features from 130 thousand collected from the official Spotify Web API. Music Analyzer takes the information from the database and can display differences, similarities, trends, and even recommendations between whole genres and specific tracks. Team 40 Plus 1 is a team formed through the INFO 201 course at the University of Washington.
")
  })
}

shinyServer(my_server)
