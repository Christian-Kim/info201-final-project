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
    # action button to clear the playlist. Also a table to show what songs are currently in the playlist. 
    sidebarPanel(
      plotlyOutput("radarchart_playlist"),
      tableOutput("playlist"),
      actionButton(
        "clear_playlist",
        "Clear your current playlist"
      )
    ),
    
    # main panel for the 2nd tab. Has subtabs inside of it which are for playlist creation and song recommendations
    mainPanel(
      tabsetPanel(type = "tabs",
        # Playlist creation subtab. Has a radar chart showing the attributes of each individual song. 
        # and a drop down to choose the tab to filter songs. Also a select tab which allows for adding of songs to playlist
        # Only adds songs when the action button is pressed. 
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
        # Song recommendation subtab. When the action button is pressed the song recommendation is updated 
        # This depends on the songs from the previous tab. 
        tabPanel(
          "Song Recommendation",
          actionButton(
            "update_recs",
            "Press to see songs similar to the ones in your playlist"
          ),
          DT::dataTableOutput("song_recommendation_table")
        )
      )
    )
  ),
  # 3rd Tab called popularity trends. Drop down menu to pick which genre to examine.
  # another dropdown menu to choose to x axis to see how each individual effects popularity. 
  # also a plot to show the trend 
  tabPanel(
    "Popularity Trends",
    textOutput("header_popularity_graph"),
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
  # 4th tab about this project tab. Displays text about this project. 
  tabPanel(
    "About this Project",
    mainPanel(
      htmlOutput("about_this_project")
    )
  )
)

shinyUI(my_ui)