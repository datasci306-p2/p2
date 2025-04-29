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
library(readr)

# Load sampled IMDb data
title_principals <- read_csv("title_principals_sample.csv.gz")
name_basics <- read_csv("name_basics_sample.csv.gz")
title_basics <- read_csv("title_basics_sample.csv.gz")

# Set Kevin Bacon's ID
kevin_bacon_id <- "nm0000102"

# Define UI
ui <- fluidPage(
  titlePanel("6 Degrees of Kevin Bacon"),
  sidebarLayout(
    sidebarPanel(
      textInput("movieTitle", "Enter Movie or TV Show Title:"),
      actionButton("startGame", "Start Game"),
      uiOutput("selectPersonUI"),
      actionButton("nextStep", "Next Step")
    ),
    mainPanel(
      textOutput("status"),
      tableOutput("peopleTable")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  game <- reactiveValues(
    current_people = NULL,
    selected_person = NULL,
    steps = 0,
    won = FALSE
  )
  
  observeEvent(input$startGame, {
    req(input$movieTitle)
    
    # Find movie
    title_id <- title_basics %>%
      filter(primaryTitle == input$movieTitle) %>%
      pull(tconst)
    
    if (length(title_id) == 0) {
      game$current_people <- NULL
      return(NULL)
    }
    
    # Find people in movie
    game$current_people <- title_principals %>%
      filter(tconst %in% title_id) %>%
      inner_join(name_basics, by = "nconst") %>%
      select(nconst, primaryName)
    
    game$steps <- 0
    game$won <- FALSE
  })
  
  output$selectPersonUI <- renderUI({
    if (is.null(game$current_people)) return(NULL)
    selectInput("personSelect", "Select a Person:", 
                choices = setNames(game$current_people$nconst, game$current_people$primaryName))
  })
  
  output$peopleTable <- renderTable({
    if (is.null(game$current_people)) return(NULL)
    game$current_people
  })
  
  observeEvent(input$nextStep, {
    req(input$personSelect)
    
    game$selected_person <- input$personSelect
    game$steps <- game$steps + 1
    
    # Find titles selected person appeared in
    titles <- title_principals %>%
      filter(nconst == game$selected_person) %>%
      pull(tconst)
    
    # Find all people in those titles
    game$current_people <- title_principals %>%
      filter(tconst %in% titles) %>%
      inner_join(name_basics, by = "nconst") %>%
      select(nconst, primaryName) %>%
      distinct()
    
    # Check for Kevin Bacon
    if (kevin_bacon_id %in% game$current_people$nconst) {
      game$won <- TRUE
    }
  })
  
  output$status <- renderText({
    if (is.null(game$current_people)) {
      return("Enter a movie title and start the game.")
    }
    if (game$won) {
      return(paste("You found Kevin Bacon in", game$steps, "steps! You win!"))
    }
    if (game$steps >= 6) {
      return("You have taken 6 steps without finding Kevin Bacon. You lose!")
    }
    paste("Step:", game$steps, "- Keep going!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)