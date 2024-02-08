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
        title = "Predation risk all years",
        status = "info",
        plotOutput(outputId = ns("predrisk_plot_allyears"))
      ),

    shinydashboard::box(
      width = 12,
      title = "Predation Risk by year",
      status = "info",

        # select locations
        selectInput(
          inputId = ns("select_loc"),
          label = "Locations",
          choices = unique(df$site),
          selected = unique(df$site),
          # width = "200px",
          multiple = T
        ),

      sliderInput(
        inputId = ns("year_slider"),
        label = "Year(s) released",
        min = min(df$year),
        max = max(df$year),
        value = c(min(df$year), max(df$year)),
        step = 1,
        sep = ""),

      plotOutput(outputId = ns("predrisk_plot_byyear"))

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
      df_pred_summary %>%
        filter(year >= input$year_slider[1] & year <= input$year_slider[2],
               site %in% c(input$select_loc))
    })

    #render plot all year
    output$predrisk_plot_allyears<- renderPlot(


      filtered_data() %>%
        ggplot(aes(x = as.factor(year), y = pct_susceptible, color = site))+
        geom_bar(stat = "identity", position = "dodge", width = .8) +
        geom_text( aes(label = paste0(round(pct_susceptible,1),"%")),
                  position = position_fill(vjust = 1)) +
        facet_wrap(~site, scales = "free") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    )


    #render plot by year
    output$predrisk_plot_byyear<- renderPlot(
      ggplot(filtered_data(), aes(x=site, y=pct_susceptible, color = site))+
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_y_continuous(labels = scales::percent_format())+
        facet_wrap(~year, ncol = 4) + theme_light()
    )
  })
}

## To be copied in the UI
# mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")

## To be copied in the server
# mod_subpage2_predrisk_server("mod_subpage2_predrisk_1")
