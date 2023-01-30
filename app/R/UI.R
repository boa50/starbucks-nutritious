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