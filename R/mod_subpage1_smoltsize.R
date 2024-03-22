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
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Smolt Size Distributions",
        HTML("The distribution of smolt fork lengths can be explored across years, months, and half-months throughout the outmigration season."),

      ),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Select factors of interest as you explore smolt size distributions:",
        mod_subpage1_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")
      ),

      shinydashboard::box(
        title = "Below displays selected size distributions in comparison to predator size thresholds:",
        status = "info",
        width = 12,
        height = "auto",
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
            HTML("Select tabs below to adjust temporal resolution:")
          )
        ),
        br(),
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
        # fluidRow(
        #   column(
        #     width = 12,
        #     h4("Additional text after tabs")
        #   )
        # )
        div(style='height:600;overflow-y: scroll;')
      )
    )
  )
}


#' subpage1_smoltsize Server Functions
#'
#' @noRd
mod_subpage1_smoltsize_server <- function(id, data, predators_selected, years_selected, plot_height, locations_selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$annual_plot <- renderPlot({

      fct_smoltsize_histogram_or_density_plot(data = data(), facet_by = "year", predators_selected = predators_selected(), years_selected = years_selected(), graph = input$select_graph, locations_selected = locations_selected())
    },
    height = plot_height())

    output$monthly_plot <- renderPlot({
      fct_smoltsize_histogram_or_density_plot(data = data(), facet_by = "by_month", predators_selected = predators_selected(), years_selected = years_selected(), graph = input$select_graph, locations_selected = locations_selected())
    },
    height = plot_height())

    output$half_monthly_plot <- renderPlot({
      fct_smoltsize_histogram_or_density_plot(data = data(), facet_by = "by_half_month", predators_selected = predators_selected(), years_selected = years_selected(), graph = input$select_graph, locations_selected = locations_selected())
    },
    height = plot_height())



    print(predators_selected)
  })
}



## To be copied in the UI
# mod_subpage1_smoltsize_ui("subpage1_smoltsize_1")

## To be copied in the server
# mod_subpage1_smoltsize_server("subpage1_smoltsize_1")
