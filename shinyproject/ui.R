library(shiny)
library(shinythemes)
library(plotly)

attributes <- c("popularity",  "instrumentalness", "acousticness", "danceability", "energy", "liveness", "speechiness", "valence")
playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)

my_ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Music Analyzer",
  tabPanel("Genre Page Thingy. Name Placeholder",
    sidebarPanel(
      uiOutput("genre_list"), 
      checkboxGroupInput(
        "attributes",
        "Select Attributes",
        attributes,
        selected = c("popularity", "energy", "valence")
      )
    ),
    mainPanel(
      plotlyOutput("radarchart_genres")
    )
  ), 
  tabPanel(
    "Playlist Stuff",
    sidebarPanel(
      tableOutput("playlist")
    ), 
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(
          "Playlist Creation",
          plotlyOutput("radarchart_playlist"),
          checkboxGroupInput(
            "attributes_playlist",
            "Select Attributes",
            attributes,
            selected = c("popularity", "energy", "valence")
          ),
          uiOutput("genre_selection"),
          uiOutput("song_selection")
        ), 
        tabPanel(
          "Song Recommendation", 
          tableOutput("song_recommendation_table")
        )
      )
    )
  )
)

shinyUI(my_ui)