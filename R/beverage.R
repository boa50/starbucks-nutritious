get_beverage_name <- function(beverage) {

  return(
    renderText(beverage()$beverage_name)
  )
}

get_beverage_img <- function(beverage, img_size = "sm") {
  stopifnot(is.reactive(beverage))
  observe({ stopifnot(is.data.frame(beverage())) })
  stopifnot(img_size %in% c("sm", "lg"))

  file_name <- reactive(
    beverage()$beverage %>%
      stringr::str_replace("\\(.+\\)" , "") %>%
      trimws() %>%
      stringr::str_replace_all(" ", "_") %>%
      tolower() %>%
      paste(".webp", sep="")
  )
  width <- ifelse(img_size == "lg", 250, 150)

  return(
    renderImage({
      list(
        src = file.path("www", file_name()),
        alt = file_name(),
        width = width
      )
    }, deleteFile = FALSE)
  )
}

### Module
beverage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(ns("beverage_img"), height = "auto"),
    span(textOutput(ns("beverage_name")))
  )
}

beverage_server <- function(id, beverage, img_size = "sm") {
  stopifnot(is.reactive(beverage))
  observe({ stopifnot(is.data.frame(beverage())) })
  stopifnot(img_size %in% c("sm", "lg"))

  moduleServer(
    id,
    function(input, output, session) {
      output$beverage_name <- get_beverage_name(beverage)
      output$beverage_img <- get_beverage_img(beverage, img_size)
    }
  )
}
