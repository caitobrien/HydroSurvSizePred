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

  #retrieve reactive values to use in plots and tables as needed
  dataselect_reactives <- mod_subpage1_smoltsize_dataselection_server("subpage1_smoltsize_dataselection_1")
  #year_display <- dataselect_reactives$year_display

  mod_subpage1_smoltsize_server("subpage1_smoltsize_1")


}
