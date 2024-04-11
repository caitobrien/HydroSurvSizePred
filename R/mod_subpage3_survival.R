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

          # #select predator thresholds
          # column(
          #   width = 3,
          #   selectInput(
          #     inputId = ns("select_predator"),
          #     label = "Predator risk",
          #     choices = unique(df_pred_summary$predator),
          #     selected = unique(df_pred_summary$predator),
          #     multiple = TRUE
          #   )
          # ),

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
        title = "Compare reach survival to predation risk:",
        status = "info",
        collapsible = TRUE,
        collapsed = TRUE,
        "pending plot",
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


    # Filter data based on slider input
    filtered_data <- reactive({
      data_surv %>%
        dplyr::filter(
          reach %in% c(input$select_reach),
         # predator %in% c(input$select_pred),
          migration %in% c(input$select_passtype),
          year %in% c(input$year_slider[1]:input$year_slider[2])
          )
    })

    #render plot
    output$survival_plot<- plotly::renderPlotly({
      fct_survival_plot(data = filtered_data())
    })

    #render mixed plot
    output$survivalpred_plot<- renderPlot({

      # Add a 'site' column to df_survival
      data_surv$site <- ifelse(data_surv$migration == "In-river", "BON", "LWG")

      ggplot2::ggplot() +
        ggplot2::geom_bar(data = dplyr::filter(data_pred_risk, dplyr::between(year, 1998,2019)),  ggplot2::aes(x = site, y = pct_susceptible, color = predator, fill = predator),
                 stat = "identity",
                 position = ggplot2::position_dodge(preserve = "single")) +
        ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        ggplot2::geom_pointrange(data = dplyr::filter(data_surv, reach == "LGR_BOA" & dplyr::between(year, 1998,2019)),
                                 ggplot2::aes(x = site, y = median, ymin = min, ymax = max, color = migration),
                        position =  ggplot2::position_dodge(width = 0.9)) +
        ggplot2::labs(x = "Site by Year",
                      y = "Percent Susceptible [Predation Risk]",
                      fill = "Predation",
                      color = "Passage Type") +
        ggplot2::scale_fill_manual(values = c("N. Pikeminnow" = "black","Pacific Hake" = "grey")) +
        ggplot2::scale_color_manual(values = c("In-river" = "steelblue4", "Transported" = "#b47747")) +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(~., name = "Estimated Percent Survival [Passage Type]")) +
        ggh4x::facet_nested(. ~ year + site, scales = "free_x", space = "free_x", switch = "x", nest_line = ggplot2::element_line(linetype = 1)) +
        ggplot2::theme_minimal()+
        ggplot2::theme(axis.text.x =  ggplot2::element_blank(),
              axis.ticks.x =  ggplot2::element_blank(),
              axis.line.x =  ggplot2::element_blank(),
              panel.spacing =  ggplot2::unit(0.5, "lines"),
              legend.position = "top"
              )

    })




  })
}

## To be copied in the UI
# mod_subpage3_survival_ui("subpage3_survival_1")

## To be copied in the server
# mod_subpage3_survival_server("subpage3_survival_1")
