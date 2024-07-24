#' subpage1_submodule_smoltsize_dataselection UI Function
#'
#' @description module specific for the data selection inputs used to generate smolt size distribution plots
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage1_submodule_smoltsize_dataselection_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      # select locations
      column(
        width = 3,
        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("select_loc"),
          label = "Location(s)",
          shape = "curve",
          outline = TRUE,
          choices = c("BON", "LWG"),
          selected =  c("BON", "LWG")
          )
        ),

      #select predator thresholds
      column(
        width = 3,
        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("select_predator"),
          label = "Predator threshold(s)",
          shape = "curve",
          outline = TRUE,
          choices = unique(predator_thresholds$species),
          selected = unique(predator_thresholds$species),
          )
        ),

      #select year
      column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("select_year"),
          label = "Select year(s)",
          choices = sort(unique(df_fish$year)),
          selected = 2019,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      )
    )
  )
}

#' subpage1_smoltsize_dataselection Server Functions
#'
#' @noRd
mod_subpage1_submodule_smoltsize_dataselection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #reactive for predator thresholds, years, and site
    predators_selected <- reactive({
      input$select_predator
    })

    years_selected<- reactive({input$select_year})

    locations_selected <- reactive({
      input$select_loc
    })

    #set reactive for plot heights
    plot_height<- reactive({
      req(years_selected())
      nyears <- length(years_selected())

      if (nyears > 3) {
        plot_height<- 400 + (nyears-3)*120
      }else plot_height <-400
    })

    # reactive for year and location selection to filter data used in plots
    filtered_data<- reactive({

      df_fish %>%
        dplyr::filter(
          site %in% c(input$select_loc),
          year %in% c(input$select_year)#year %in% c(input$year_slider[1]:input$year_slider[2])
          )
    })

    # Return the reactive expression(s)
    return(list(
      filtered_data = reactive(filtered_data),
      predators_selected = reactive(predators_selected),
      plot_height = reactive(plot_height),
      years_selected = reactive(years_selected),
      locations_selected = reactive(locations_selected)
    ))
  })
}


## To be copied in the UI
# mod_subpage1_submodule_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")

## To be copied in the server
# mod_subpage1_submodule_smoltsize_dataselection_server("subpage1_smoltsize_dataselection_1")
