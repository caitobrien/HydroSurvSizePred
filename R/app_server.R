#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_welcome_page_server("welcome_page_1")

  mod_main_page_server("main_page_1")

  mod_background_page_server("background_page_1")


}
