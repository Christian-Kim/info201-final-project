library(shiny)
library(shinythemes)
library(plotly)
library(DT)

attributes <- c("popularity",  "instrumentalness", "acousticness", "danceability", "energy", "liveness", "speechiness", "valence")
playlist <- read.csv("./playlist.csv", stringsAsFactors = FALSE)


my_ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Music Analyzer",
  # This is the first tab, called Feature Analysis by Genre
  tabPanel("Feature Analysis by Genre",
           
    # this is the header text output for the first tab
    textOutput("header1"), 
    
    # side bar panel for Feature Analysis by Genre tab
    sidebarPanel(
      div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("genre_list")), 
      div(style="display: inline-block;vertical-align:top; width: 150px;", checkboxGroupInput(
        "attributes",
        "Select Attributes",
        attributes,
        selected = attributes
      ))
    ),
    
    #Main panel for the Feature Analysis by Genre tab
    mainPanel(
      plotlyOutput("radarchart_genres")
    )
  ), 
  
  # 2nd Tab, named Create a playlist tab
  tabPanel(
    "Create a Playlist",
    
    # Renders header text for this tab
    textOutput("header2"),
    # sidebar Panel layout for the 2nd tab, has the radar chart for the playlist as a whole and the 
    # action button for the clear playlist
    sidebarPanel(
      plotlyOutput("radarchart_playlist"),
      tableOutput("playlist"),
      actionButton(
        "clear_playlist",
        "Clear your current playlist"
      )
    ),
    
    # main panel for the 2nd playlist
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
          ),
          textOutput("header_playlist_creation")
        ),
        # Describe this tab...
        tabPanel(
          "Song Recommendation",
          textOutput("song_recommendation_text_output"),
          actionButton(
            "update_recs",
            "Press to see songs similar to the ones in your playlist"
          ),
          DT::dataTableOutput("song_recommendation_table")
        )
      )
    )
  ),
  # Describe this tab...
  tabPanel(
    "Popularity Trends",
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