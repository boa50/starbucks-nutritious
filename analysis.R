library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

### Question: Can you identify the most nutritious items on the menu?

df <- read_csv("dataset/DataDNA Dataset Challenge -- January 2023.csv")
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

get_daily_value <- function(nutrient, value) {
  return(nutrient)
}

pivot_longer(df[15,], 4:18)[, c("name", "value")] %>% 
  filter(name %in% important_nutrients) %>% 
  arrange(match(name, important_nutrients)) %>% 
  mutate(dv = get_daily_value(name))

### Creating coffee name texts
df$beverage_name <- df$beverage_prep %>% 
  str_replace("^(Short|Tall|Grande|Venti|Solo|Doppio)", "") %>% 
  trimws() %>% 
  paste(df$beverage, " (", ., ")", sep = "") %>% 
  trimws() %>% 
  str_replace(" \\(\\)", "")
