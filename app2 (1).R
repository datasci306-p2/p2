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

## Application 2:

# Load sampled IMDb data
title_principals <- read_csv("title_principals_sample.csv.gz")
name_basics <- read_csv("name_basics_sample.csv.gz")

# Define UI
ui <- fluidPage(
  titlePanel("Filter IMDb People by Category and Job"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categorySelect", 
                  "Select Category:", 
                  choices = unique(title_principals$category)),
      uiOutput("jobSelect")
    ),
    mainPanel(
      tableOutput("personTable")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$jobSelect <- renderUI({
    availableJobs <- title_principals %>%
      filter(category == input$categorySelect) %>%
      pull(job) %>%
      unique()
    
    selectInput("jobSpecific", 
                "Select Specific Job:", 
                choices = availableJobs)
  })
  
  output$personTable <- renderTable({
    title_principals %>%
      filter(category == input$categorySelect, job == input$jobSpecific) %>%
      inner_join(name_basics, by = "nconst") %>%
      select(primaryName, birthYear)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)