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
      div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("genre_list")), 
      div(style="display: inline-block;vertical-align:top; width: 150px;", checkboxGroupInput(
        "attributes",
        "Select Attributes",
        attributes,
        selected = c("popularity", "energy", "valence")
      ))
    ),
    mainPanel(
      plotlyOutput("radarchart_genres")
    )
  ), 
  tabPanel(
    "Playlist Stuff",
    sidebarPanel(
      tableOutput("playlist"),
      actionButton(
        "clear_playlist",
        "Clear your current playlist"
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(
          "Playlist Creation",
          div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxGroupInput(
            "attributes_playlist",
            "Select Attributes",
            attributes,
            selected = c("popularity", "energy", "valence")
          )),
          div(style="display: inline-block;vertical-align:top; width: 500px;", plotlyOutput("radarchart_playlist")),
          uiOutput("genre_selection"),
          uiOutput("song_selection"),
          actionButton(
            "update_playlist",
            "Add this song to your playlist"
          )
        ), 
        tabPanel(
          "Song Recommendation", 
          tableOutput("song_recommendation_table")
        )
      )
    )
  ),
  tabPanel(
    "Data Comparison",
    sidebarPanel(
      uiOutput(
        "genres_drop_down"),
      selectInput(
        "x",
        "X-Axis",
        attributes[-1]
      )
    ),
    mainPanel(
      plotOutput("outputPlot")
    )
  ),
  tabPanel("About this Project")
)

shinyUI(my_ui)