important_nutrients <- c(
  "calories",
  "total_carbohydrates_g",
  "total_fat_g",
  "saturated_fat_g",
  "trans_fat_g",
  "cholesterol_mg",
  "protein_g",
  "sugars_g",
  "sodium_mg",
  "caffeine_mg"
)

get_daily_value <- function(df) {
  dv <- case_when(
    df$name == "calories" ~ df$value / 2000,
    df$name == "total_carbohydrates_g" ~ df$value / 300,
    df$name == "total_fat_g" ~ df$value / 80,
    df$name == "saturated_fat_g" ~ df$value / 30,
    df$name == "trans_fat_g" ~ df$value / 5,
    df$name == "cholesterol_mg" ~ df$value / 200,
    df$name == "protein_g" ~ df$value / 50,
    df$name == "sugars_g" ~ df$value / 35,
    df$name == "sodium_mg" ~ df$value / 2300,
    df$name == "caffeine_mg" ~ df$value / 400,
    TRUE ~ df$value
  )
  
  dv <- round(dv * 100) 
  
  return(dv)
}

get_representative_name <- function(df) {
  representative_names <- case_when(
    df$name == "calories" ~ "Calories",
    df$name == "total_carbohydrates_g" ~ "Carbohydrates (g)",
    df$name == "total_fat_g" ~ "Total Fat (g)",
    df$name == "saturated_fat_g" ~ "Saturated Fat (g)",
    df$name == "trans_fat_g" ~ "Trans Fat (g)",
    df$name == "cholesterol_mg" ~ "Cholesterol (mg)",
    df$name == "protein_g" ~ "Protein (g)",
    df$name == "sugars_g" ~ "Sugars (g)",
    df$name == "sodium_mg" ~ "Sodium (mg)",
    df$name == "caffeine_mg" ~ "Caffeine (mg)",
    TRUE ~ df$name
  )
  
  return(representative_names)
}

get_nutritional_table <- function(beverage) {
  stopifnot(is.data.frame(beverage))
  
  df_nutrients <- pivot_longer(beverage, 4:18)[, c("name", "value")] %>%
    filter(name %in% important_nutrients) %>% 
    arrange(match(name, important_nutrients)) %>%
    mutate(`Daily Value` = get_daily_value(.),
           `Daily Value` = ifelse(value == 0, 
                                  "-", 
                                  paste(`Daily Value`, "%", sep = "")),
           Nutrient = get_representative_name(.),
           Quantity = as.character(value)) %>% 
    select("Nutrient", "Quantity", "Daily Value")
}

get_nutritional_table_title <- function(beverage) {
  stopifnot(is.data.frame(beverage))
  
  paste("<strong>", beverage$beverage_name, "Nutrients </strong>")
}

### Module
nutritionalTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("nutritional_table_title")),
    tableOutput(ns("nutritional_table")),
    p("Daily values are calculated based on a 2000 calories diet", 
      style = "color: #9e9e9e;")
  )
}

nutritionalTableServer <- function(id, beverage) {
  stopifnot(is.reactive(beverage))
  
  moduleServer(
    id,
    function(input, output, session) {
      output$nutritional_table_title <- renderText({
        stopifnot(is.data.frame(beverage()))
        
        get_nutritional_table_title(beverage())
      })
      
      output$nutritional_table <- renderTable({
        stopifnot(is.data.frame(beverage()))
        
        get_nutritional_table(beverage())
      })
    }
  )
}

### Testing the module
# library(shiny)
# source("dataset.R")
# 
# df <- reactive(get_df())
# 
# ui <- fluidPage(
#   nutritionalTableUI("nut_table")
# )
# 
# server <- function(input, output, session) {
#   beverage <- reactive(df()[1, ])
#   nutritionalTableServer("nut_table", beverage)
# }
# 
# shinyApp(ui, server)