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
        HTML("A comparison between smolt size distributions, predation risk, and estimated LWG-BOA survival across years."),
        collapsible = TRUE,
        collapsed = FALSE
      ),

      shinydashboard::box(
        width = 12,
        title = "Select factors of interest as you explore:",
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
          shinyWidgets::pickerInput(
            inputId = ns("select_year"),
            label = "Select Year(s)",
            choices = sort(unique(df_fish$year)),
            selected = 2000,
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          )
        )
      ),

      shinydashboard::box(
        width = 12,
        height = "850px",
        title = "Display summary of size distribution, predation risk, and estimate survival by year:",
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

     #set reactive for plot heights
     plot_height<- reactive({
         req(years_selected())
       nyears <- length(years_selected())

       if (nyears > 3) {
         plot_height<- 500 + (nyears-3)*150
       }else plot_height <-500
     })

     #set reactive for size plot
    df_size_filtered<-reactive({
      data_size %>%
        dplyr::filter(site %in% c(input$select_site),
               year %in% c(input$select_year))
    })

  #set reactive for theshold lines on size plot
    df_pred_threshold_filtered<-reactive({
      data_pred_threshold%>%
        dplyr::filter(species %in% c(input$select_predator))
    })

  #set reactive for pred risk plot
    df_pred_risk_filtered<-reactive({
      data_pred_risk%>%
        dplyr::filter(predator %in% c(input$select_predator),
               site %in% c(input$select_site),
               year %in% c(input$select_year))
    })

  # set reactive for pct_survival
    df_surv_filtered<-reactive({
      data_surv%>%
        dplyr::filter(migration %in% c(input$select_passtype),
               year %in% c(input$select_year))
    })

    output$summary_plot<- renderPlot({

      # Create an empty list to store plots
      plots_list <- list()

      # Loop through selected years and create plots
      for (i in seq_along(input$select_year)) {
        year_selected <- input$select_year[i]

        # Filter data for the current year
        data_size_year <- df_size_filtered() %>% dplyr::filter(year == year_selected)
        data_pred_risk_year <- df_pred_risk_filtered() %>% dplyr::filter(year == year_selected)
        data_surv_year <- df_surv_filtered() %>% dplyr::filter(year == year_selected)

        plots_list[[as.character(year_selected)]] <- fct_summary_plot(
          data_size = data_size_year,
          data_pred_threshold = df_pred_threshold_filtered(),
          data_pred_risk = data_pred_risk_year,
          data_surv = data_surv_year,
          year = year_selected,
          show_legend = i == 1  # Show legend only for the first plot
        )
      }

      # Arrange plots side by side using patchwork
      if (length(plots_list) > 0) {
        top_legends <- patchwork::wrap_plots(plots_list, ncol = 1)
        print(top_legends)
      } else {
        # Handle case when no years are selected
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data selected")
      }
      } , height = function() {
        plot_height()
      })

  })
}

## To be copied in the UI
# mod_subpage4_summary_ui("subpage4_summary_1")

## To be copied in the server
# mod_subpage4_summary_server("subpage4_summary_1")
