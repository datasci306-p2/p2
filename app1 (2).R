#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

## Application 1:

# Load sampled IMDb data
title_basics <- read_csv("title_basics_sample.csv.gz")
title_ratings <- read_csv("title_ratings_sample.csv.gz")

# Merge basics and ratings
imdb_data <- inner_join(title_basics, title_ratings, by = "tconst")

# Define UI
ui <- fluidPage(
  titlePanel("IMDb Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", 
                  "Select Year Range:", 
                  min = min(imdb_data$startYear, na.rm = TRUE), 
                  max = max(imdb_data$startYear, na.rm = TRUE), 
                  value = c(2000, 2020)),
      selectInput("genreSelect", 
                  "Select Genres:", 
                  choices = unique(unlist(strsplit(paste(imdb_data$genres, collapse = ","), ","))),
                  selected = "Drama",
                  multiple = TRUE),
      checkboxInput("highRating", 
                    "Only include movies with averageRating > 7", 
                    value = FALSE)
    ),
    mainPanel(
      plotOutput("ratingPlot"),
      plotOutput("genrePlot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filteredData <- reactive({
    data <- imdb_data %>%
      filter(startYear >= input$yearRange[1], startYear <= input$yearRange[2]) %>%
      filter(sapply(strsplit(genres, ","), function(x) any(x %in% input$genreSelect)))
    
    if (input$highRating) {
      data <- data %>% filter(averageRating > 7)
    }
    data
  })
  
  output$ratingPlot <- renderPlot({
    filteredData() %>%
      group_by(startYear) %>%
      summarise(avg_rating = mean(averageRating, na.rm = TRUE)) %>%
      ggplot(aes(x = startYear, y = avg_rating)) +
      geom_line(color = "blue") +
      labs(title = "Average Rating Over Time", x = "Year", y = "Average Rating")
  })
  
  output$genrePlot <- renderPlot({
    filteredData() %>%
      separate_rows(genres, sep = ",") %>%
      filter(genres %in% input$genreSelect) %>%
      count(genres) %>%
      ggplot(aes(x = reorder(genres, -n), y = n, fill = genres)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Number of Movies by Genre", x = "Genre", y = "Number of Movies")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)