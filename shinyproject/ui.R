library(shiny)
library(shinythemes)
library(plotly)
library(DT)

attributes <- c("popularity",  "instrumentalness", "acousticness", "danceability", "energy", "liveness", "speechiness", "valence")
playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)

my_ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Music Analyzer",
  # Describe this tab...
  tabPanel("Genre Page Thingy. Name Placeholder",
    sidebarPanel(
      div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("genre_list")), 
      div(style="display: inline-block;vertical-align:top; width: 150px;", checkboxGroupInput(
        "attributes",
        "Select Attributes",
        attributes,
        selected = attributes
      ))
    ),
    mainPanel(
      plotlyOutput("radarchart_genres")
    )
  ), 
  # Describe this tab...
  tabPanel(
    "Playlist Stuff",
    sidebarPanel(
      plotlyOutput("radarchart_playlist"),
      tableOutput("playlist"),
      actionButton(
        "clear_playlist",
        "Clear your current playlist"
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        # Describe this tab...
        tabPanel(
          "Playlist Creation",
          div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxGroupInput(
            "attributes_playlist",
            "Select Attributes",
            attributes,
            selected = attributes
          )),
          div(style="display: inline-block;vertical-align:top; width: 500px;", plotlyOutput("radarchart_selected_song")),
          uiOutput("genre_selection"),
          uiOutput("song_selection"),
          actionButton(
            "update_playlist",
            "Add this song to your playlist"
          )
        ),
        # Describe this tab...
        tabPanel(
          "Song Recommendation", 
          DT::dataTableOutput("song_recommendation_table")
        )
      )
    )
  ),
  # Describe this tab...
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
  # Describe this tab...
  tabPanel(
    "About this Project",
    mainPanel("Welcome")
  )
)

shinyUI(my_ui)