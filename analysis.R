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