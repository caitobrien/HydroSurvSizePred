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
    shinydashboard::box(
      "Select Predator(s) and year of interest:",
        # select rear type
      shinyWidgets::prettyCheckboxGroup(
            inputId = ns("select_rear"),
            label = "Rearing type(s)",
            shape = "curve",
            outline = TRUE,
            choices = unique(df_fish$t_rear_type),
            selected = NULL
          ),

        # select passage type
      shinyWidgets::prettyCheckboxGroup(
          inputId = ns("select_pass"),
          label = "Passage type(s)",
          shape = "curve",
          outline = TRUE,
          choices = unique(df_fish$pass_type_T_R),
          selected = NULL
        ),

        #select predator thresholds
        # column(
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("select_predator"),
            label = "Predator threshold(s)",
            shape = "curve",
            outline = TRUE,
            choices = unique(predator_thresholds$species),
            selected = NULL
          ),

        #select year
          shinyWidgets::pickerInput(
            inputId = ns("select_year"),
            label = "Select year",
            choices = sort(unique(df_fish$year)),
            selected = 2019,
            options = list(`actions-box` = TRUE),
            multiple = FALSE
          # )
        )
      )
    ),

    column(
      width = 9,
      plotOutput(ns("histogram"))
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
          t_rear_type  %in% c(input$select_rear),
          pass_type_T_R %in% c(input$select_pass)
        )
    })

    output$histogram <- renderPlot({

        plot <- ggplot(filtered_data(), aes(x = length)) +
          geom_histogram(binwidth = 5, fill = "white", color = "black", size = 1) +
          facet_grid(t_rear_type~pass_type_T_R) +
          labs(title = "Smolt size distribution at LWG with highlighted predator size thresholds") +
          theme_minimal()

        selected_predators <- input$select_predator
        for (predator in selected_predators) {
          threshold <- predator_thresholds$median[predator_thresholds$species == predator]
          if (!is.null(threshold) && any(filtered_data()$length < threshold)) {
            plot <- plot +
              geom_histogram(data = subset(filtered_data(), length < threshold), binwidth = 5, fill = "black", color = "white", alpha=.3) +
              geom_text(data = subset(filtered_data(), length < threshold),
                        aes(x = threshold - 10, y = 10, label = paste0(round(sum(length < threshold) / nrow(filtered_data()) * 100, 2), "%")),
                        color = "black", size = 4)
          }
        }

        plot

    })
  })
}



## To be copied in the UI
# mod_subpage2_submodule_predrisk_smoltsize_plot_ui("subpage2_submodule_predrisk_smoltsize_plot_1")

## To be copied in the server
# mod_subpage2_submodule_predrisk_smoltsize_plot_server("subpage2_submodule_predrisk_smoltsize_plot_1")
