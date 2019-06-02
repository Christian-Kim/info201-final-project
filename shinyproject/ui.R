library(shiny)
library(shinythemes)
library(plotly)

my_ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Music Analyzer"),
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
)

shinyUI(my_ui)