library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Preparing the dataset
df <- read_csv("dataset/DataDNA Dataset Challenge -- January 2023.csv")

df <- clean_names(df)

df$caffeine_mg <- as.numeric(df$caffeine_mg)

df <- mutate_all(df, ~ifelse(is.na(.), 0, .))

df$size <- str_extract(df$beverage_prep, "^(Short|Tall|Grande|Venti|Solo|Doppio)")
df <- df %>% fill(size, .direction = "down")

df <- df %>% 
  mutate(
    bad_fat_g = trans_fat_g + saturated_fat_g,
    good_fat_g = total_fat_g - bad_fat_g
  )

df$beverage_name <- df$beverage_prep %>% 
  str_replace("^(Short|Tall|Grande|Venti|Solo|Doppio)", "") %>% 
  trimws() %>% 
  paste(df$beverage, " (", ., ")", sep = "") %>% 
  trimws() %>% 
  str_replace(" \\(\\)", "")

df_scores <- df %>%  filter(size %in% c("Tall", "Solo", "Doppio"))


### Creating the dashboard
controls_slider <- function(name) {
  column(
    1,
    sliderInput(
      tolower({{ name }}), 
      paste({{ name }}, ":", sep = ""), 
      min = -1, 
      max = 1, 
      value = 0, 
      step = 0.05, 
      ticks = FALSE
    )
  )
}

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
        column(
          12,
          imageOutput("nutritious_first_img", height = "auto"),
          p(textOutput("nutritious_first_name"))
        )
      ),
      fluidRow(
        column(
          12,
          br(),
          p(strong("Other interesting options for you"))
        )
      ),
      fluidRow(
        column(
          6,
          imageOutput("nutritious_second_img", height = "auto"),
          p(textOutput("nutritious_second_name"))
        ),
        column(
          6,
          imageOutput("nutritious_third_img", height = "auto"),
          p(textOutput("nutritious_third_name"))
        ),
      ),
    ),
    column(
      3, offset = 1,
      htmlOutput("nutritious_table_title"),
      tableOutput("nutritional_table"),
      p("Daily values are calculated based on a 2000 calories diet", 
        style = "color: #9e9e9e;")
    )
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

get_beverage_name <- function(df_reactive, position) {
  return(
    renderText(
      df_reactive()[{{ position }}, ] %>% pull("beverage_name")
    )
  )
}

get_beverage_img <- function(df_reactive, position) {
  file_name <- reactive(
    df_reactive()[{{ position }}, ]$beverage %>% 
      str_replace("\\(.+\\)" , "") %>% 
      trimws() %>% 
      str_replace_all(" ", "_") %>% 
      tolower() %>% 
      paste(".webp", sep="")
  )
  width <- ifelse(position == 1, 250, 150)
  
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
  dv <- ifelse(df$name == "calories", df$value / 2000,
        ifelse(df$name == "total_carbohydrates_g", df$value / 300, 
        ifelse(df$name == "total_fat_g", df$value / 80,
        ifelse(df$name == "saturated_fat_g", df$value / 30,
        ifelse(df$name == "trans_fat_g", df$value / 5,
        ifelse(df$name == "cholesterol_mg", df$value / 200,
        ifelse(df$name == "protein_g", df$value / 50,
        ifelse(df$name == "sugars_g", df$value / 35,
        ifelse(df$name == "sodium_mg", df$value / 2300,
        ifelse(df$name == "caffeine_mg", df$value / 400,
               df$value))))))))))
  
  dv <- round(dv * 100) 
  
  return(dv)
}

get_representative_name <- function(df) {
  representative_names <- ifelse(df$name == "calories", "Calories",
                          ifelse(df$name == "total_carbohydrates_g", "Carbohydrates (g)",
                          ifelse(df$name == "total_fat_g", "Total Fat (g)",
                          ifelse(df$name == "saturated_fat_g", "Saturated Fat (g)",
                          ifelse(df$name == "trans_fat_g", "Trans Fat (g)",
                          ifelse(df$name == "cholesterol_mg", "Cholesterol (mg)",
                          ifelse(df$name == "protein_g", "Protein (g)",
                          ifelse(df$name == "sugars_g", "Sugars (g)",
                          ifelse(df$name == "sodium_mg", "Sodium (mg)",
                          ifelse(df$name == "caffeine_mg", "Caffeine (mg)",
                                 df$name))))))))))
  
  return(representative_names)
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
  
  output$nutritious_first_name <- get_beverage_name(df_updated, 1)
  output$nutritious_second_name <- get_beverage_name(df_updated, 2)
  output$nutritious_third_name <- get_beverage_name(df_updated, 3)
  
  output$nutritious_first_img <- get_beverage_img(df_updated, 1)
  output$nutritious_second_img <- get_beverage_img(df_updated, 2)
  output$nutritious_third_img <- get_beverage_img(df_updated, 3)
  
  output$nutritious_table_title <- renderText(
    paste("<strong>", df_updated()[1, ]$beverage_name, "Nutrients </strong>")
  )
  
  output$nutritional_table <- renderTable({
    df_nutrients <- pivot_longer(df_updated()[1,], 4:18)[, c("name", "value")] %>%
      filter(name %in% important_nutrients) %>% 
      arrange(match(name, important_nutrients)) %>%
      mutate(`Daily Value` = get_daily_value(.),
             `Daily Value` = ifelse(value == 0, 
                                    "-", 
                                    paste(`Daily Value`, "%", sep = "")),
             Nutrient = get_representative_name(.),
             Quantity = as.character(value))
    
    df_nutrients[, c("Nutrient", "Quantity", "Daily Value")]
  })
}

shinyApp(ui = ui, server = server)