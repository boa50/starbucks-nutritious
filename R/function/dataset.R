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
    dplyr::filter(size %in% c("Tall", "Doppio"))  
  
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

get_beverage_by_size <- function(beverage_name, beverage_size) {
  stopifnot(is.character(beverage_name))
  stopifnot(is.character(beverage_size))
  
  beverage <- df %>% 
    filter(beverage_name == .env$beverage_name &
             size == .env$beverage_size)
    
  return(beverage)
}

get_beverage_sizes <- function(beverage_name) {
  sizes <- df %>% 
    filter(beverage == .env$beverage_name) %>%
    pull("size") %>%
    unique()
  
  return(sizes)
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
  
  df_scores <- get_df_scores() %>% 
    dplyr::mutate(
      score = (total_carbohydrates_g / 300) * carbohydrates + # FDA
        ifelse(fat > 0, 
               (good_fat_g / 50 - trans_fat_g / 5 - saturated_fat_g / 20),
               total_fat_g / 80) * fat +
        # (good_fat_g / 50 - trans_fat_g / 5 - saturated_fat_g / 20) * fat + # UK government
        (protein_g / 50) * protein + # UK government
        (sugars_g / 35) * sugar + # AHA
        (sodium_mg / 2300) * sodium + # AHA
        (caffeine_mg / 400) * caffeine # FDA
    )
  
  return(
    df_scores[order(df_scores$score, decreasing = TRUE), ]
  )
}