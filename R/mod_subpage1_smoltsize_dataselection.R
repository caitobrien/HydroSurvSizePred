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

    fluidRow(
      # select locations
      column(
        width = 3,
        selectInput(
          inputId = ns("select_loc"),
          label = "Locations",
          choices = unique(data.test$site),
          selected = unique(data.test$site),
          multiple = TRUE
          )
        ),

      #select predator thresholds
      column(
        width = 3,
        selectInput(
          inputId = ns("select_predator"),
          label = "Predator thresholds",
          choices = unique(predator_thresholds$species),
          selected = unique(predator_thresholds$species),
          multiple = TRUE
          )
        ),

      #select year
      column(
        width = 3,
          sliderInput(
            inputId = ns("year_slider"),
            label = "Year(s) released",
            min = min(df$year),
            max = max(df$year),
            value = c(min(df$year), max(df$year)),
            step = 1,
            sep = "")
      )
    )
  )
}

#' subpage1_smoltsize_dataselection Server Functions
#'
#' @noRd
mod_subpage1_smoltsize_dataselection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #reactive for predator thresholds
    predators_selected <- reactive({
      input$select_predator
    })

    # reactive for year and location selection to filter data used in plots
    filtered_data<- reactive({

      df %>%
        dplyr::filter(
          site %in% c(input$select_loc),
          year %in% c(input$year_slider[1]:input$year_slider[2])
          )
    })

    # Return the reactive expression(s)
    return(list(
      filtered_data = reactive(filtered_data),
      predators_selected = reactive(predators_selected)
    ))
  })
}


## To be copied in the UI
# mod_subpage1_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")

## To be copied in the server
# mod_subpage1_smoltsize_dataselection_server("subpage1_smoltsize_dataselection_1")
