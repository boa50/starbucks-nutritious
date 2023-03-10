library(shiny)
library(magrittr)
library(shinyWidgets)
library(bslib)

addResourcePath('www', system.file('www', package = 'starbucksNutritious'))

starbucks_app <- function() {
  app_theme <- bs_theme(
    version = 5,
    bg = "#1f3933",
    fg = "#FDF7F7",
    primary = "#1f3933"
  ) %>%
    bs_add_rules(sass::sass_file("www/styles.scss"))

  ui <- fluidPage(
    theme = app_theme,

    fluidRow(
      column(
        7,
        titlePanel("What is the most nutritious beverage?")
      ),
      column(
        1,
        img(
          src = file.path("www", "starbucks_logo.webp"),
          width = 50,
          style = "padding-top: 10px;"
        )
      )
    ),
    fluidRow(
      hr()
    ),

    fluidRow(
      column(
        4, align = "center",
        fluidRow(
          column(12, beverage_ui("beverage_1"))
        ),
        fluidRow(
          column(
            12,
            br(),
            p(strong("Other interesting options for you"))
          )
        ),
        fluidRow(
          column(6, beverage_ui("beverage_2")),
          column(6, beverage_ui("beverage_3")),
        ),
      ),
      column(3, offset = 1, nutritional_table_ui("nut_table"))
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

  server <- function(input, output) {
    df_beverages <- reactive(
      calculate_scores(
        carbohydrates = input$carbohydrates,
        fat = input$fat,
        protein = input$protein,
        sugar = input$sugar,
        sodium = input$sodium,
        caffeine = input$caffeine
      )
    )

    beverage_server("beverage_1", get_beverage(df_beverages, 1), img_size = "lg")
    beverage_server("beverage_2", get_beverage(df_beverages, 2))
    beverage_server("beverage_3", get_beverage(df_beverages, 3))

    nutritional_table_server("nut_table", get_beverage(df_beverages, 1))
  }

  shinyApp(ui = ui, server = server)
}
