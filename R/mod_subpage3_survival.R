#' subpage3_survival UI Function
#'
#' @description module for layout and function of fish survival
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage3_survival_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Estimated Survival by Reach",
        HTML("Predicted survival from LWG-BON (downstream survival), BON-BOA (estuary & ocean survival), and LWG-BOA using CJS modeling. Survival of transported fish from LGR-BON is assumed to be 1."),
        collapsible = TRUE,
        collapsed = FALSE
      ),

      shinydashboard::box(
        width = 12,
        title = "Select reach, passage type and year to explore predicted survival:",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,

          # select reach
          column(
            width = 3,
            selectInput(
              inputId = ns("select_reach"),
              label = "Reaches",
              choices = unique(df_survival$reach),
              selected = unique(df_survival$reach),
              multiple = TRUE
            )
          ),

        #select passage type
        column(
          width = 3,
          selectInput(
            inputId = ns("select_passtype"),
            label = "Passage type",
            choices = unique(df_survival$migration),
            selected = unique(df_survival$migration),
            multiple = TRUE
          )
        ),

          #select year
          column(
            width = 3,
            sliderInput(
              inputId = ns("year_slider"),
              label = "Year(s) released",
              min = min(df_survival$year),
              max = max(df_survival$year),
              value = c(min(df_survival$year), max(df_survival$year)),
              step = 1,
              sep = "")
          )
        ),

      shinydashboard::box(
        width = 12,
        title = "Reach survival by passage type and year:",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        plotly::plotlyOutput(outputId = ns("survival_plot"), height = "800px")
      ),

      shinydashboard::box(
        width = 12,
        title = "Compare LGR - BOA reach survival to predation risk:",
        status = "info",
        collapsible = TRUE,
        collapsed = TRUE,
        #select predator thresholds
          selectInput(
            inputId = ns("select_predator"),
            label = "Select predator risk",
            choices = unique(df_pred_summary$predator),
            selected = "Pacific Hake",
            multiple = TRUE
          ),
        plotOutput(outputId = ns("survivalpred_plot"))
      )
    )
  )
}

#' subpage3_survival Server Functions
#'
#' @noRd
mod_subpage3_survival_server <- function(id, data_surv, data_pred_risk){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Filter survival data based on slider input
    filtered_data_surv <- reactive({
      data_surv %>%
        dplyr::filter(
          reach %in% c(input$select_reach),
          migration %in% c(input$select_passtype),
          year %in% c(input$year_slider[1]:input$year_slider[2])
          )
    })

    #render plot
    output$survival_plot<- plotly::renderPlotly({
      fct_survival_plot(data = filtered_data_surv())
    })

    filtered_data_pred <- reactive({
      data_pred_risk %>%
      dplyr::filter(predator %in% c(input$select_predator))
    })

    #render mixed plot
    output$survivalpred_plot<- renderPlot({



      fct_survival_predrisk_plot(data_surv = df_survival, data_pred_risk = filtered_data_pred())

    })




  })
}

## To be copied in the UI
# mod_subpage3_survival_ui("subpage3_survival_1")

## To be copied in the server
# mod_subpage3_survival_server("subpage3_survival_1")
