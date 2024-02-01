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
      title = "Predation Risk by year",
      status = "info",

      sliderInput(
        inputId = ns("year_slider"),
        label = "Year(s) released",
        min = min(data$year),
        max = max(data$year),
        value = c(min(data$year), max(data$year)),
        step = 1,
        sep = ""),

      plotOutput(outputId = ns("predrisk_plot_allyears"))

    )
    )
  )
}

#' mod_subpage2_predrisk Server Functions
#'
#' @noRd
mod_subpage2_predrisk_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Filter data based on slider input
    filtered_data <- reactive({
      data %>%
        filter(year >= input$year_slider[1] & year <= input$year_slider[2])
    })


    #render plot
    output$predrisk_plot_allyears<- renderPlot(
      ggplot(filtered_data(), aes(x=location, y=pctprey, color = predator))+
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_wrap(~year, ncol = 4) + theme_light()
    )
  })
}

## To be copied in the UI
# mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")

## To be copied in the server
# mod_subpage2_predrisk_server("mod_subpage2_predrisk_1")
