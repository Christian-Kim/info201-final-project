library(shiny)
library(shinythemes)
library(plotly)

my_ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Music Analyzer",
  tabPanel("Genre Page Thingy. Name Placeholder",
    sidebarPanel(
      uiOutput("genre_list"), 
      checkboxGroupInput(
        "attributes",
        "Select Attributes",
        c("popularity",  "instrumentalness", "acousticness", "danceability", "energy", "liveness", "speechiness", "valence"),
        selected = c("popularity", "energy", "valence")
      )
    ),
    mainPanel(
      plotlyOutput("radarchart")
    )
  ), 
  tabPanel("Tab #2 name place holder"
    #sidebarPanel(),
    ##mainPanel()
  )
)

shinyUI(my_ui)