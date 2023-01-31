df <- readr::read_csv("data/DataDNA Dataset Challenge -- January 2023.csv")

df <- janitor::clean_names(df)

df$caffeine_mg <- as.numeric(df$caffeine_mg)

df <- dplyr::mutate_all(df, ~ifelse(is.na(.), 0, .))

df$size <- stringr::str_extract(df$beverage_prep, "^(Short|Tall|Grande|Venti|Solo|Doppio)")
df <- df %>% tidyr::fill(size, .direction = "down")

df <- df %>% 
  dplyr::mutate(
    bad_fat_g = trans_fat_g + saturated_fat_g,
    good_fat_g = total_fat_g - bad_fat_g
  )

df$beverage_name <- df$beverage_prep %>% 
  stringr::str_replace("^(Short|Tall|Grande|Venti|Solo|Doppio)", "") %>% 
  trimws() %>% 
  paste(df$beverage, " (", ., ")", sep = "") %>% 
  trimws() %>% 
  stringr::str_replace(" \\(\\)", "")

get_df_scores <- function() {
  df_scores <- df %>% 
    dplyr::filter(size %in% c("Tall", "Solo", "Doppio"))  
  
  return(df_scores)
}

get_beverage <- function(df, position = 1) {
  stopifnot(is.reactive(df))
  observe({ stopifnot(is.data.frame(df())) })
  stopifnot(position > 0)
  
  return(
    reactive(df()[{{ position }}, ])
  )
}

calculate_scores <- function(
    carbohydrates = 0,
    fat = 0,
    protein = 0,
    sugar = 0,
    sodium = 0,
    caffeine = 0) {
  
  stopifnot(is.numeric(carbohydrates))
  stopifnot(is.numeric(fat))
  stopifnot(is.numeric(protein))
  stopifnot(is.numeric(sugar))
  stopifnot(is.numeric(sodium))
  stopifnot(is.numeric(caffeine))
  
  df_scores <- get_df_scores()
  
    df_scores$score <- (
      # FDA
      (df_scores$total_carbohydrates_g / 300) * carbohydrates
      # UK government
      + (df_scores$good_fat_g / 50 - df_scores$bad_fat_g / 30) * fat
      # UK government
      + (df_scores$protein_g / 50) * protein
      # AHA
      + (df_scores$sugars_g / 35) * sugar
      # AHA
      + (df_scores$sodium_mg / 2300) * sodium
      # FDA
      + (df_scores$caffeine_mg / 400) * caffeine
    )
    
    return(df_scores[order(df_scores$score, decreasing = TRUE), ])
}