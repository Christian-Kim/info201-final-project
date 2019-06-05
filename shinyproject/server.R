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
}

shinyServer(my_server)
