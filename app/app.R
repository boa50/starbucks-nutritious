library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

source("R/dataset.R")
source("R/UI.R")
source("modules/nutritional_table.R")
source("modules/beverage.R")

df <- get_df()
df_scores <- get_df_scores()

ui <- fluidPage(
  fluidRow(
    column(
      7,
      titlePanel("What is the most nutritious beverage?")
    ),
    column(
      1,
      p(),
      imageOutput("logo", height = "auto")
    )
  ),
  fluidRow(
    hr()
  ),
  
  fluidRow(
    column(
      4, align = "center",
      fluidRow(
        column(12, beverageUI("beverage_1"))
      ),
      fluidRow(
        column(
          12,
          br(),
          p(strong("Other interesting options for you"))
        )
      ),
      fluidRow(
        column(6, beverageUI("beverage_2")),
        column(6, beverageUI("beverage_3")),
      ),
    ),
    column(3, offset = 1, nutritionalTableUI("nut_table"))
  ),
  fluidRow(
    br(),
    column(12, p(strong("What is important to you?")))
  ),
  fluidRow(
    controls_slider("Carbohydrates"),
    controls_slider("Fat"),
    controls_slider("Protein"),
    controls_slider("Sugar"),
    controls_slider("Sodium"),
    controls_slider("Caffeine"),
    column(
      2, 
      p("All images were obtained from https://www.starbucks.com/menu",
        style = "color: #9e9e9e;")
    )
  )
)

get_beverage <- function(df, position) {
  reactive(df()[{{ position }}, ])
}

server <- function(input, output) {
  df_updated <- reactive({
    df_scores$score <- (
      # FDA
      (df_scores$total_carbohydrates_g / 300) * input$carbohydrates
      # UK government
      + (df_scores$good_fat_g / 50 - df_scores$bad_fat_g / 30) * input$fat
      # UK government
      + (df_scores$protein_g / 50) * input$protein
      # AHA
      + (df_scores$sugars_g / 35) * input$sugar
      # AHA
      + (df_scores$sodium_mg / 2300) * input$sodium
      # FDA
      + (df_scores$caffeine_mg / 400) * input$caffeine
    )
    
    return(df_scores[order(df_scores$score, decreasing = TRUE), ])
  })
  
  output$logo <- renderImage({ 
    list(
      src = file.path("img", "starbucks_logo.webp"),
      width = 50
    )
  }, deleteFile = FALSE)
  
  beverageServer("beverage_1", get_beverage(df_updated, 1), img_size = "lg")
  beverageServer("beverage_2", get_beverage(df_updated, 2))
  beverageServer("beverage_3", get_beverage(df_updated, 3))
  
  nutritionalTableServer("nut_table", get_beverage(df_updated, 1))
}

shinyApp(ui = ui, server = server)