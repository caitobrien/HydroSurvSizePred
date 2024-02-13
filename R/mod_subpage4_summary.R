#' subpage4_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage4_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Summary of size distribution, predation risk, and estimated survival",
        HTML("<b>Length, predation risk, survival:</b> A comparison between length distributions predation risk, and LWG-BOA survival is available for viewing across years."),
        collapsible = TRUE,
        collapsed = TRUE
      ),

      shinydashboard::box(
        width = 12,
        title = "Select inputs",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,

        # select site
        column(
          width = 3,
          selectInput(
            inputId = ns("select_site"),
            label = "Location",
            choices = unique(df_fish$site),
            selected = unique(df_fish$site),
            multiple = TRUE
          )
        ),

        #select predator thresholds
        column(
          width = 3,
          selectInput(
            inputId = ns("select_predator"),
            label = "Predator risk",
            choices = unique(df_pred_summary$predator),
            selected = unique(df_pred_summary$predator),
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
          selectInput(
            inputId = ns("select_year"),
            label = "Year released",
            choices = unique(df_fish$year),
            selected = 2000,
            multiple = TRUE
            )
        )
      ),

      shinydashboard::box(
        width = 12,
        height = "850px",
        title = "Summary by year",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,

        plotOutput(outputId = ns("summary_plot"))
      )
    )
  )
}

#' subpage4_summary Server Functions
#'
#' @noRd
mod_subpage4_summary_server <- function(id, data_size, data_pred_threshold, data_pred_risk, data_surv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

     years_selected<- reactive({input$select_year})

    df_size_filtered<-reactive({
      data_size %>%
        filter(site %in% c(input$select_site),
               year %in% c(input$select_year))

    })

    df_pred_threshold_filtered<-reactive({
      data_pred_threshold%>%
        filter(species %in% c(input$select_predator))
    })

    df_pred_risk_filtered<-reactive({
      data_pred_risk%>%
        filter(predator %in% c(input$select_predator),
               site %in% c(input$select_site),
               year %in% c(input$select_year))
    })

    df_surv_filtered<-reactive({
      data_surv%>%
        filter(migration %in% c(input$select_passtype),
               year %in% c(input$select_year))
    })


    output$summary_plot<- renderPlot({
      #fct_summary_plot(data_size = df_size_filtered(), data_pred_threshold = df_pred_threshold_filtered(), data_pred_risk = df_pred_risk_filtered(), data_surv = df_surv_filtered(), year = years_selected() )
      # Create an empty list to store plots
      plots_list <- list()

      # Loop through selected years and create plots
      for (year in input$select_year) {
        plots_list[[as.character(year)]] <- fct_summary_plot(
          data_size = df_size_filtered(),
          data_pred_threshold = df_pred_threshold_filtered(),
          data_pred_risk = df_pred_risk_filtered(),
          data_surv = df_surv_filtered(),
          year = year
        )
      }

      # Arrange plots side by side using patchwork
      if (length(plots_list) > 0) {
        top_legends <- wrap_plots(plots_list, ncol = 1)
        print(top_legends)
      } else {
        # Handle case when no years are selected
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data selected")
      }
      })

  })
}

## To be copied in the UI
# mod_subpage4_summary_ui("subpage4_summary_1")

## To be copied in the server
# mod_subpage4_summary_server("subpage4_summary_1")
