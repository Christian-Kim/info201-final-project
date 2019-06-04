library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)

files <- dir("./data")
data <- do.call(rbind,lapply(paste0("./data/", files), read.csv, stringsAsFactors = FALSE))
data <- mutate(data, popularity = popularity / 100.0, genre = data[,1])

genre_list <- as.vector(distinct(data, genre)[,1])

radar_chart <- function(group_names, df, columns) {
  p <- plot_ly(type = 'scatterpolar', mode = 'marker', fill = 'toself')
  for (i in seq(length(group_names))) {
    p <- add_trace(p, r = as.numeric(df[i,]), theta = columns, name = group_names[i])
  }
  p <- layout(p, polar = list(radialaxis = list(visible = T, range = c(0, 100))))
  return (p)
}

get_genre_stats <- function(genres, input_df, columns) {
  df <- data.frame(matrix(ncol = length(columns), nrow = length(genres)))
  colnames(df) <- columns
  if (length(genres) == 0) {
    return (radar_chart(c(), df, columns))
  }
  for (i in seq(length(genres))) {
    for (j in seq(length(columns))) {
      col_sym <- rlang::sym(columns[j])
      genre_df <- filter(data, genre == genres[i])
      summarized_col <- summarize(genre_df, mean(!!col_sym))
      df[i, j] <- summarized_col[1,1] * 100
    }
  }
  return (radar_chart(genres, df, columns))
}

my_server <- function(input, output) {
  output$genre_list <- renderUI({
    checkboxGroupInput(
      "genres",
      "Select Genres",
      genre_list,
      selected = c("Pop", "Comedy")
    )
  })
  
  output$radarchart <- renderPlotly({
    return (get_genre_stats(input$genres, data, input$attributes))
  })
}

shinyServer(my_server)
