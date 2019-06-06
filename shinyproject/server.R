library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)


## Lines of code in order to manipulate our data into a useable format
files <- dir("./data")
music_data <- do.call(rbind,lapply(paste0("./data/", files), read.csv, stringsAsFactors = FALSE))
music_data <- mutate(music_data, popularity = popularity / 100.0,
               genre = music_data[,1],
               songBy = paste(track_name, "by", artist_name))

distinct_songs <- distinct(music_data, track_name, .keep_all = TRUE)

## list of the genres 
genre_list <- as.vector(distinct(music_data, genre)[,1])

## Creates a radar chart where each element in group name displays as a trace,
## df contains one row per trace, columns define which attributes are shown, and
## W, H, and M define the display sizes of the chart.
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

## this is to initialize an empty playlist for the start of the app 
playlist <- music_data %>% head(1) %>% filter(artist_name == "")
write.csv(playlist, "./playlist.csv", row.names = FALSE)

## initiates the server 
my_server <- function(input, output) {
  output$genre_list <- renderUI({
    checkboxGroupInput(
      "genres",
      "Select Genres",
      genre_list,
      selected = c("Pop", "Comedy")
    )
  })
  
  ## renders the radar chart for the attributes and genres in the first tab which is based on the selected attributes and 
  ## genres
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
  
  ## renders the select dropdown menu in the 2nd tab, playlist creation subtab, which displays the genre to pick. 
  output$genre_selection <- renderUI({
    selectizeInput(
      "genre_selection",
      "Select a genre to add songs from",
      choices = genre_list,
      selected = "Pop"
    )
  })

  ## renders the table in the 2nd tab that displays the playlist created. Only updates when the action button
  ## update_playlist is pressed.
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
  
  ## clears the playlist and updates the table showing the playlist when the clear playlist button is pressed to an empty table. 
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
  
  ## Renders the select input in the playlist creation subtab for the songs to put into the playlist
  ## only shows songs that are within the selected genre in the other dropdown menu.
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
  
  ## Anytime the add button songto playlist button is pressed of the clear playlist button is pressed, 
  ## This renders a new radar plot showing the average attributes of the given playlist. 
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
  
  ## Whenever a new song is picked in the dropdown menu containing the songs, a new radar plot is displayed 
  ## in the 2nd tab, playlist creation screen showing the attributes of the selected song. 
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
  
  ## renders the plot for the for the 3rd tab depending on the input of the drop down menu in the sidebar 
  ## Of the selected genre and selected attribute in the dropdown menus in the sidebar. 
  output$outputPlot <- renderPlot({
    test1 <- filter(music_data, genre == toString(input$list_of_genres_plot))
    plot <- ggplot(test1, aes(x = get(input$x), y = popularity)) +
              geom_smooth() +
              labs(x = input$x, y = "popularity", title = paste(input$x, "vs popularity for", input$list_of_genres_plot, "Music")) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(plot)
  })
  
  ## Renders the drop down menu of all the genres in our dataset in the sidebar 
  output$genres_drop_down <- renderUI({
    selectInput("list_of_genres_plot", "Pick a Genre", genre_list)
  })
  
  ## Renders the table in the song recommendation tab based on the playlist created. 
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
  
  ## Renders the text in the tab Feature Analysis by Genre. 
  output$header1 <- renderText({
    return(HTML("<font size=\"+1\"> Here the user is presented with checkboxes to select genres and attributes. 
Once specific genres and attributes have been selected the data is taken and formatted into a radar chart 
which will display the levels of each attribute and categorize each genre by color. 
This layout grants an efficient way to view many different attributes of many different genres. 
It allows a user to analyze the overall attributes of genres relative to other genres. Each feature's weight has been
scaled by a factor of 100 to make the data readable if it was originally scaled from 0 to 1. </font> <br/><br/><br/>"))
  })
  
  ## renders the text in the Create a Playlist tab. 
  output$header2 <- renderText({
    return(HTML("<font size=\"+1\"> Here the user can select between two different subtabs. The first subtab Playlist Creation allows the user search for 
specific tracks within a specific genre. Once a user has selected a track the Music Analyzer present that individual track's radar chart 
containing the levels for attributes that can be selected to the left of the radar chart. If the user likes the track they can push the 
Add this song to your playlist button to add the song into your playlist. 
All the songs in the user's playlist are displayed in a sidebar panel with a radar chart displaying 
the average attribute data from all the songs in the playlist. Once the user is satisfied with their playlist 
they can then go to the Song Recommendation subtab and press the Press to see songs similar to the ones in your playlist button 
to see the tracks from the data with the most similar attribute levels. 
These features can be used to assist the user by introducing tracks that they are likely to enjoy. </font> <br/><br/>"))
  })
  
  ## renders the text in the popularity graph tab. 
  output$header_popularity_graph <- renderText({
    return(HTML("<font size=\"+1\"> Here the user can choose a genre and an attribute to plot.
Once the genre and attributes have been selected, the plot will filter and select the corresponding tracks from the data
and will plot the data according to the popularity. This can be used to analyze what attributes of specific genres
result in a popular/successful track. </font>"))
  })
  
  ## renders the text in the about this project tab
  output$about_this_project <- renderText({
    return(HTML("<font size=\"+2\"> The design team 40 Plus 1 would like to introduce our newest app, Music Analyzer.
<br/> Running as a potential extension of Spotify, Music Analyzer uses data from kaggle user tomigelo's Spotify Audio Features
which contains audio features from 130 thousand unique songs collected from the official Spotify Web API.
<br/> Music Analyzer takes the information from the database and can display differences, similarities, trends, and even recommendations
between whole genres and specific tracks.
<br/>
<br/> The motivation behind this application is to both analyze how specific features in songs affect them, and also provide individual users
with insights into their own musical tastes.
<br/>
<br/> Team 40 Plus 1 is a team formed through the INFO 201 course at the University of Washington.
<br/>
<br/> Team Members:
<br/> Ryan Qiu
<br/> Christian Kim
<br/> John Lee
</font>"))
  })
  
  ## renders the text in the What are the features? tab
  output$feature_descriptions <- renderText({
    return(HTML("<br/><br/> <font size=\"+1\"> <strong> artist_name: </strong> <br/> Shows the name of the artist excluding featured artists 
                
<br/><br/> <strong> track_name: </strong> <br/> Shows the name of the track, includes featured artists

<br/><br/> <strong> acousticness: </strong> <br/> Is a confidence measure from 0.0 to 1.0 of whether the track is acoustic (not having electrical amplification).

<br/><br/> <strong> danceability: </strong> <br/> Is a confidence measure from 0.0 to 1.0 that describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability/consistency, and beat strength.

<br/><br/> <strong> energy: </strong> <br/> Is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.

<br/><br/> <strong> instrumentalness: </strong> <br/> The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.

<br/><br/> <strong> liveness: </strong> <br/> Is the level of detection of an audience in the track (higher liveness indicates a higher likelihood of the track having been played live.  

<br/><br/> <strong> speechiness: </strong> <br/> Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.

<br/><br/> <strong> valence: </strong> <br/> A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive, while tracks with low valence sound more negative.

<br/><br/> <strong> popularity: </strong> <br/> The popularity of a track is a value between 0 and 100, with 100 being the most popular. The popularity is calculated by algorithm and is based, in the most part, on the total number of plays the track has had and how recent those plays are.
            
<br/><br/> Source: <a href=\"https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/\">Spotify API</a> </font>"))
  })
}

shinyServer(my_server)
