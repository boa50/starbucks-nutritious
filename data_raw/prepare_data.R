library(magrittr)

df <- vroom::vroom("data_raw/DataDNA Dataset Challenge -- January 2023.csv")

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

vroom::vroom_write(df, "data/beverages.tsv")

