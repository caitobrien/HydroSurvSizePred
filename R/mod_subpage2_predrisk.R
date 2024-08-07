#' mod_subpage2_predrisk UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage2_predrisk_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Predation risk",
        "The percentages of smolts below size thresholds susceptible to predation from Pacific Hake and Northern Pikeminnow can be viewed across years."
      ),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Predation risk by predator",
        fluidRow(
          column(
            width = 12,
            HTML("<p>The figure below displays the predation risk based on Pacific Hake size thresholds across years. For both figures, click on the plot legend to toggle the display of locations</p>")
          ),
          column(
            width = 10,
            offset = 1, # Centering the column
            plotly::plotlyOutput(outputId = ns("plot_hake"),height = "50%")
          )
        ),
        br(),
        "Northern Pikeminnow have a larger prey size threshold, therefore most, if not all, smolt are at risk of predation based on size.",
        br(),
        fluidRow(
          column(
            width = 10,
            offset = 1, # Centering the column
            plotly::plotlyOutput(outputId = ns("plot_pike"), height = "50%")
          )
        ),
        br(),
        HTML("The proportions of fish below a size threshold were calculated following Muir et al. (2006) and the size distributions of N. Pikeminnow (Winther et al. 2021) and Pacific Hake (Taylor et al. 205) observed each year. See <b>Supplementary Materials</b> for more details."
        )
      ),

      shinydashboard::box(width = 12,
                          title = "Interactive display of predation risk on smolt size distribution",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = TRUE,
                          mod_subpage2_submodule_predrisk_smoltsize_plot_ui("subpage2_submodule_predrisk_smoltsize_plot_1")
                          )
      # shinydashboard::box(width = 12,
      #                     title = "Add numbers and distribution (interactive map?) and simple figure?",
      #                     status = "info",
      #                     collapsible = TRUE,
      #                     collapsed = TRUE)
    )
  )
}

#' mod_subpage2_predrisk Server Functions
#'
#' @noRd
mod_subpage2_predrisk_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #selective plot showing
    output$plot_hake <- plotly::renderPlotly({
        filtered_data <- subset(data, predator == "Pacific Hake")
        fct_predrisk_bar_plot(data = filtered_data)
    })

    #selective plot showing
    output$plot_pike <- plotly::renderPlotly({
        filtered_data <- subset(data, predator == "N. Pikeminnow")
        fct_predrisk_bar_plot(data = filtered_data)
    })

  })
}

## To be copied in the UI
# mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")

## To be copied in the server
# mod_subpage2_predrisk_server("mod_subpage2_predrisk_1")
