#' subpage1_smoltsize UI Function
#'
#' @description Module to designate general layout of subpage1_smolt size
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage1_smoltsize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Select factors of interest as you explore smolt length distributions:",
        mod_subpage1_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")
      ),

      shinydashboard::box(
        title = "Plots",
        status = "info",
        width = 12,
        fluidRow(
          column(
             width = 3, # Adjust width as needed
            offset = 9, # Center the radio buttons
            radioButtons(
              inputId = ns("select_graph"),
              label = "Choose a graph",
              choices = c("Histogram", "Density"),
              selected = "Histogram",
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            "Select tabs below to adjust temporal resolution:"
          )
        ),
        tabsetPanel(
          id = "myTabs",
          tabPanel("Annual",
                   plotOutput(
                     outputId = ns("annual_plot"))
                   ),
          tabPanel("Monthly",
                   plotOutput(
                     outputId = ns("monthly_plot"))
                   ),
          tabPanel("Half-monthly",
                   plotOutput(
                     outputId = ns("half_monthly_plot")))
        ),
        fluidRow(
          column(
            width = 12,
            h4("Additional text after tabs")
          )
        )
      )
    )
  )
}


#' subpage1_smoltsize Server Functions
#'
#' @noRd
mod_subpage1_smoltsize_server <- function(id, data, predators_selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$annual_plot <- renderPlot({
      fct_smoltsize_histogram_plot(data = data(), facet_by = "year", predators_selected = predators_selected())
    })

    output$monthly_plot <- renderPlot({
      fct_smoltsize_histogram_plot(data = data(), facet_by = "by_month", predators_selected = predators_selected())
    })

    output$half_monthly_plot <- renderPlot({
      fct_smoltsize_histogram_plot(data = data(), facet_by = "by_half_month", predators_selected = predators_selected())
    })



    print(predators_selected)
  })
}



## To be copied in the UI
# mod_subpage1_smoltsize_ui("subpage1_smoltsize_1")

## To be copied in the server
# mod_subpage1_smoltsize_server("subpage1_smoltsize_1")
