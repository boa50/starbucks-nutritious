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

card_icon <- function(name) {
  name <- name %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_to_lower()

  div(img(src = file.path("www/icons/", paste(name, "_icon.svg", sep = "")),
          alt = paste(name, "_icon", sep = ""),
          height = "50",
          width = "50"))
}

nutrient_value_box <- function(nutrient_name, values) {
  value_box(
    title = nutrient_name,
    value = span(values[1], class = "h2"),
    span(paste("(",values[2]," DV)", sep = "")),
    showcase = card_icon(nutrient_name),
    showcase_layout = showcase_top_right(max_height = "80px"),
    class = "nopad"
  )
}
