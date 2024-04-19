#' subpage2_submodule_predrisk_smoltsize_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage2_submodule_predrisk_smoltsize_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 3,
      "Select factors of interest:",

      #select year
      shinyWidgets::pickerInput(
        inputId = ns("select_year"),
        label = "Year",
        choices = sort(unique(df_fish$year)),
        selected = 2019,
        options = list(`actions-box` = TRUE),
        multiple = FALSE
        # )
      ),

        # select passage type
      shinyWidgets::prettyCheckboxGroup(
          inputId = ns("select_pass"),
          label = "Passage type(s)",
          shape = "curve",
          outline = TRUE,
          choices = unique(df_fish$pass_type_T_R),
          selected = "In-river"
        ),

        #select predator thresholds
        # column(
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("select_predator"),
            label = "Predator threshold(s)",
            shape = "curve",
            outline = TRUE,
            choices = c("Pacific Hake", "N. Pikeminnow"), #unique(predator_thresholds$species),
            selected = NULL
          )
    ),

    column(
      width = 9,
      plotOutput(ns("histogram")),
      htmlOutput(ns("percentage_text"))
    )
    )
  )
}

#' subpage2_submodule_predrisk_smoltsize_plot Server Functions
#'
#' @noRd
mod_subpage2_submodule_predrisk_smoltsize_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_data <- reactive({
      df_fish %>%
        dplyr::filter(
          site == "LWG", #%in% c(input$select_loc) include if want to add bon sites
          year == input$select_year,
          pass_type_T_R %in% c(input$select_pass)
        )
    })

    output$histogram <- renderPlot({

      selected_pass_types <- input$select_pass
      selected_predators <- input$select_predator

      fct_predrisk_smoltsize_plot(data_size = filtered_data(), predator_thresholds = predator_thresholds, selected_pass_types = selected_pass_types, selected_predators = selected_predators)

    })

    #hoover text
    output$percentage_text <- renderUI({

      selected_predators <- input$select_predator
      selected_year <- input$select_year
      selected_pass_types <- input$select_pass

      fct_predrisk_smoltsize_text(data_size = filtered_data(), predator_thresholds = predator_thresholds, selected_pass_types = selected_pass_types, selected_predators = selected_predators, selected_year = selected_year)

    })

  })
}



## To be copied in the UI
# mod_subpage2_submodule_predrisk_smoltsize_plot_ui("subpage2_submodule_predrisk_smoltsize_plot_1")

## To be copied in the server
# mod_subpage2_submodule_predrisk_smoltsize_plot_server("subpage2_submodule_predrisk_smoltsize_plot_1")
