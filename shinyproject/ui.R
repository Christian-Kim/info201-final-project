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
      plotlyOutput("radarchart")
    )
  ), 
  tabPanel("Playlist Generator"
    #sidebarPanel(),
    ##mainPanel()
  ),
  tabPanel("What makes a song popular?",
           sidebarLayout(
             sidebarPanel(
               selectInput("XAxis", "X-Axis", attributes), 
               selectInput("YAxis", "Y-Axis", attributes)
             ), 
             mainPanel(
               plotOutput("outputPlot")
             )
           )
           
    
  )
)

shinyUI(my_ui)