#' subpage1_smoltsize_dataselection UI Function
#'
#' @description module specific for the data selection inputs used to generate smolt size distribution plots
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage1_smoltsize_dataselection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 3,
      # select locations
      selectInput(
        inputId = ns("select_loc"),
        label = HTML("Locations <br>"),
        choices = c("Lower Granite (LWG)", "Bonneville (BON)"),
        selected = "Lower Granite (LWG)",
        # width = "200px",
        multiple = T
      )
    ),
    column(
      width = 3,
      # select predators
      selectInput(
        inputId = ns("select_pred"),
        label = "Include predators' prey size threshold",
        choices = c("N. Pikeminnow", "Pacific Hake"),
        selected = "Pacific Hake",
        # width = "200px",
        multiple = T
      )
    ),
    column(
      width = 3,
      # prompt to select all years or by year
      selectInput(
        inputId = ns("year_display"),
        label = "View by",
        choices = c("All Years", "Year"),
        selected = "All Years"
      ),
      uiOutput(ns("year_picker"))
    )
  )
}

#' subpage1_smoltsize_dataselection Server Functions
#'
#' @noRd
mod_subpage1_smoltsize_dataselection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the UI for the year picker
    output$year_picker <- renderUI({
      ns <- session$ns

      if (input$year_display == "Year") {
        sliderInput(
          inputId = ns("select_years"),
          label = "Year released",
          min = 1993,
          max = 2022,
          value = c(2017, 2022),
          sep = ""
        )
      } else {
        NULL
      }
    })

    # # Reactive for year_display
    # year_display <- reactive({
    #   input$year_display
    # })

  })
}


## To be copied in the UI
# mod_subpage1_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")

## To be copied in the server
# mod_subpage1_smoltsize_dataselection_server("subpage1_smoltsize_dataselection_1")
