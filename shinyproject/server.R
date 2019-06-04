library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)

files <- dir("./data")
music_data <- do.call(rbind,lapply(paste0("./data/", files), read.csv, stringsAsFactors = FALSE))
music_data <- mutate(music_data, popularity = popularity / 100.0,
               genre = music_data[,1],
               songBy = paste(track_name, "by", artist_name))

genre_list <- as.vector(distinct(music_data, genre)[,1])

radar_chart <- function(group_names, df, columns) {
  p <- plot_ly(type = 'scatterpolar', mode = 'marker', fill = 'toself')
  for (i in seq(length(group_names))) {
    p <- add_trace(p, r = as.numeric(df[i,]), theta = columns, name = group_names[i])
  }
  p <- layout(p, polar = list(radialaxis = list(visible = T, range = c(0, 100))))
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

playlist <- data.frame(Songs = c(""))
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
    return (get_genre_stats(input$genres, music_data, input$attributes))
  })
  
  output$genre_selection <- renderUI({
    selectizeInput(
      "genre_selection",
      "Select a genre to add songs from",
      choices = genre_list,
      selected = "Pop"
    )
  })
  
  output$playlist <- renderTable({
    playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
    playlist <- add_row(playlist, Songs = c(input$playlist_select))
    write.csv(playlist, "./playlist.csv", row.names = FALSE)
    return (playlist)
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
  
  output$radarchart_playlist <- renderPlotly({
    playlist_df <- filter(music_data, songBy %in% input$playlist_select)
    columns <- input$attributes_playlist

    summarized_df <- data.frame(matrix(ncol = length(columns), nrow = 1))
    colnames(summarized_df) <- columns
    
    for (j in seq(length(columns))) {
      col_sym <- rlang::sym(columns[j])
      summarized_col <- summarize(playlist_df, mean(!!col_sym))
      summarized_df[1, j] <- summarized_col[1,1] * 100
    }
    return (radar_chart(c("Your Playlist Features:"), summarized_df, columns))
  })
  
  output$radarchart <- renderPlotly({
    return (get_genre_stats(input$genres, music_data, input$attributes))
  })
  
  output$song_recommendation_table <- renderTable({
    
    return(NULL)
  })
  
  output$placeholder <- renderText({
    input$updateButton
    remove_songs <- input$playlist_songs_checkbox 
    playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
    for(remove in remove_songs) {
      playlist <- filter(playlist, Songs != remove)
    }
    return("")
  })
  
  read_playlist <- reactive({
    playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)
    return(as.vector(select(playlist, Songs)))
  })
  
  output$delete_songs <- renderUI({
    playlist_songs <- read_playlist()
    checkboxGroupInput("playlist_songs_checkbox", "Playlist Songs", playlist_songs)
    actionButton("updateButton", "Remove Songs")
  })
  
  output$outputPlot <- renderPlot({
    test1 <- filter(music_data, genre == toString(input$list_of_genres_plot))
    plot <- ggplot(test1, aes(x = get(input$x), y = popularity)) + geom_smooth() + labs(x = input$x, y = "popularity", title = paste(input$x, "vs popularity for", input$list_of_genres_plot, "Music")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot)
  })
  
  output$genres_drop_down <- renderUI({
    selectInput("list_of_genres_plot", "Pick a Genre", genre_list)
  })
}

shinyServer(my_server)
