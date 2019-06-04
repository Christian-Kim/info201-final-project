library(shiny)
library(shinythemes)
library(plotly)

attributes <- c("popularity",  "instrumentalness", "acousticness", "danceability", "energy", "liveness", "speechiness", "valence")

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
      checkboxGroupInput(
        "attributes_playlist",
        "Select Attributes",
        attributes,
        selected = c("popularity", "energy", "valence")
      )
    ), 
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("1", 
                 plotlyOutput("radarchart_playlist"), 
                 uiOutput("genre_selection"),
                 uiOutput("song_selection"),
                 dataTableOutput("playlist")), 
        tabPanel("2")
      )
      
      
    )
  )
)

shinyUI(my_ui)