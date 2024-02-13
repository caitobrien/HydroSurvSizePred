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
        title = "Predation risk",
        HTML("<b>Predation risk:</b> The percentages of smolts below size thresholds susceptible to fish predation can be viewed. The percentages were calculated following Muir et al. (2006) and the size distributions of N. Pikeminnow (Winther et al. 2021) and Pacific Hake (Taylor et al. 2015) observed each year."),
        collapsible = TRUE,
        collapsed = TRUE
      ),

      shinydashboard::box(
        width = 12,
        height = "850px",
        title = "Predation risk all years",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        # select locations
            selectInput(
              inputId = ns("select_pred"),
              label = "Select predator(s)",
              choices = unique(df_pred_summary$predator),
              selected = unique(df_pred_summary$predator),
              width = "400px",
              multiple = T
            ),
        plotly::plotlyOutput(outputId = ns("predrisk_plot_allyears"))
      ),

      shinydashboard::box(width = 12,
                          title = "Add interactive size distribution and % predation risk",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = TRUE),
      shinydashboard::box(width = 12,
                          title = "Add numbers and distribution (interactive map?) and simple figure?",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = TRUE)
    )
  )
}

#' mod_subpage2_predrisk Server Functions
#'
#' @noRd
mod_subpage2_predrisk_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Filter data based on slider input
    filtered_data <- reactive({
     data %>%
        filter(predator %in% c(input$select_pred))
    })

    #render plot all year
    output$predrisk_plot_allyears<- plotly::renderPlotly({
      fct_predrisk_bar_plot(data = filtered_data())
    })

  })
}

## To be copied in the UI
# mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")

## To be copied in the server
# mod_subpage2_predrisk_server("mod_subpage2_predrisk_1")
