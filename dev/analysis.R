library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Question: Can you identify the most nutritious items on the menu?

df <- read_csv("data_raw/DataDNA Dataset Challenge -- January 2023.csv")
str(df)

### Renaming columns
df <- clean_names(df)

### Converting caffeine column to double
df$caffeine_mg <- as.numeric(df$caffeine_mg)

### Getting rid of NA values
colSums(is.na(df))

# Setting with 0, because if we don't have the information, we expect that
# there isn't any quantity of the nutrient
df <- mutate_all(df, ~ifelse(is.na(.), 0, .))

### Setting a column with coffees' sizes.
# available_sizes <- c("Short", "Tall", "Grande", "Venti")
# available_expresso_sizes <- c("Solo", "Doppio")
df$size <- str_extract(df$beverage_prep, "^(Short|Tall|Grande|Venti|Solo|Doppio)")
df <- df %>% 
  fill(size, .direction = "down")

### Calculating good and bad fats values
df <- df %>% 
  mutate(
    bad_fat_g = trans_fat_g + saturated_fat_g,
    good_fat_g = total_fat_g - bad_fat_g
  )

### Creating a heatmap of nutrients
pivot_longer(df[15,], 4:18) %>% 
  ggplot(aes(y = name, x = 0, fill = value, label = value)) +
  geom_tile() +
  geom_text(colour = "white")

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

### Getting daily value
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

### Getting representative names
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

nutritional_table <- pivot_longer(df[1,], 4:18)[, c("name", "value")] %>% 
  filter(name %in% important_nutrients) %>% 
  arrange(match(name, important_nutrients)) %>%
  mutate(`Daily Value` = get_daily_value(.),
         `Daily Value` = ifelse(value == 0, 
                                "-", 
                                paste(`Daily Value`, "%", sep = "")),
         Nutrient = as.character(get_representative_name(.)),
         Quantity = value)

### Format nutrient value
nutrient <- nutritional_table %>% 
  filter(grepl('Caffeine', Nutrient, fixed = TRUE))

case_when(
  grepl("(g)", nutrient$Nutrient, fixed = TRUE) ~ paste(nutrient$value, "g"),
  grepl("(mg)", nutrient$Nutrient, fixed = TRUE) ~ paste(nutrient$value, "mg"),
  TRUE ~ as.character(nutrient$value)
)

get_nutrient_values <- function(nutritional_table, nutrient_name) {
  nutrient <- nutritional_table %>% 
    filter(grepl(nutrient_name, Nutrient, fixed = TRUE))
  
  quantity <- case_when(
    grepl("(g)", nutrient$Nutrient, fixed = TRUE) ~ paste(nutrient$value, "g", sep = ""),
    grepl("(mg)", nutrient$Nutrient, fixed = TRUE) ~ paste(nutrient$value, "mg", sep = ""),
    TRUE ~ as.character(nutrient$value)
  )
  
  daily_value <- nutrient %>% pull(`Daily Value`)
  
  return(c(quantity, daily_value))
}

get_nutrient_values(nutritional_table, "Carbo")

### Creating coffee name texts
df$beverage_name <- df$beverage_prep %>% 
  str_replace("^(Short|Tall|Grande|Venti|Solo|Doppio)", "") %>% 
  trimws() %>% 
  paste(df$beverage, " (", ., ")", sep = "") %>% 
  trimws() %>% 
  str_replace(" \\(\\)", "")


### Getting beverage sizes
test_name <- df[14, ]$beverage
test_name <- "Brewed Coffee"
df %>% 
  filter(beverage == test_name) %>% 
  pull("size") %>% 
  unique()