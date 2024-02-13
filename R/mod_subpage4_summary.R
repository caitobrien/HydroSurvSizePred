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
            selected = unique(df_fish$year),
            multiple = FALSE
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
mod_subpage4_summary_server <- function(id, data_size, data_pred){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sites_selected <- reactive({input$select_site})
    preds_selected<- reactive({input$select_predator})
    passage_selected<- reactive({input$select_passtype})
    years_selected<- reactive({input$select_year})

    df_size_filtered<-reactive({
      data_size %>%
        filter(site %in% c(input$select_site))

    })

    df_pred_filtered<-reactive({
      data_pred%>%
        filter(predator %in% c(input$select_predator))
    })


    output$summary_plot<- renderPlot({
      df_size_filtered() %>%
        ggplot(aes(x = length)) +
        geom_histogram(aes(fill = site), binwidth = 5) +
        geom_vline(data = filter(df_pred_filtered(),  type == "median"), aes(color = predator, xintercept = threshold, linetype = type )) +
        scale_fill_manual(values = c("LWG" = "steelblue4", "BON" = "#b47747"),
                          labels = c("LWG", "BON")) +
        scale_color_manual(values = c("N. Pikeminnow" = "darkgreen", "Pacific Hake" = "goldenrod"),
                           labels = c("darkgreen" = "N. Pikeminnow", "goldenrod" = "Pacific Hake")) +
        labs(
          x = "Fork length (mm)",
          y= "Number of smolt",
          fill = "Location",
          color = "Predator median",
          linetype = NULL) +
        guides(linetype = FALSE,
               color = FALSE) +
        facet_wrap(~year)
    })

  })
}

## To be copied in the UI
# mod_subpage4_summary_ui("subpage4_summary_1")

## To be copied in the server
# mod_subpage4_summary_server("subpage4_summary_1")
