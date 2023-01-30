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

get_df <- function() {
  return(df)
}

get_df_scores <- function() {
  df_scores <- df %>% 
    filter(size %in% c("Tall", "Solo", "Doppio"))  
  
  return(df_scores)
}

