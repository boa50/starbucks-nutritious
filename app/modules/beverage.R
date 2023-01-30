get_beverage_name <- function(beverage) {
  stopifnot(is.reactive(beverage))
  
  return(
    renderText(beverage()$beverage_name)
  )
}

get_beverage_img <- function(beverage, img_size = "sm") {
  stopifnot(is.reactive(beverage))
  stopifnot(is.character(img_size))
  
  file_name <- reactive(
    beverage()$beverage %>% 
      str_replace("\\(.+\\)" , "") %>% 
      trimws() %>% 
      str_replace_all(" ", "_") %>% 
      tolower() %>% 
      paste(".webp", sep="")
  )
  width <- ifelse(img_size == "lg", 250, 150)
  
  return(
    renderImage({ 
      list(
        src = file.path("img", file_name()),
        alt = file_name(),
        width = width
      )
    }, deleteFile = FALSE)
  )
}

### Module
beverageUI <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(ns("beverage_img"), height = "auto"),
    p(textOutput(ns("beverage_name")))
  )
}

beverageServer <- function(id, beverage, img_size = "sm") {
  stopifnot(is.reactive(beverage))
  stopifnot(is.character(img_size))
  
  moduleServer(
    id,
    function(input, output, session) {
      output$beverage_name <- get_beverage_name(beverage)
      output$beverage_img <- get_beverage_img(beverage, img_size)
    }
  )
}

### Testing the module
# library(shiny)
# source("dataset.R")
# df <- reactive(get_df())
# 
# ui <- fluidPage(
#   beverageUI("test")
# )
# 
# server <- function(input, output, session) {
#   beverageServer("test", df, 1)
# }
# 
# shinyApp(ui, server)