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
        title = "Select factors of interest as you explore smolt length distributions:",
        mod_subpage1_smoltsize_dataselection_ui("subpage1_smoltsize_dataselection_1")
      ),

      shinydashboard::box(
        title = "Plots",
        status = "info",
        width = 12,
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
            "Select tabs below to adjust temporal resolution:"
          )
        ),
        tabsetPanel(
          id = "myTabs",
          tabPanel("Annual",
                   plotOutput(
                     outputId = ns("annual_plot"))
                   ),
          tabPanel("Monthly"),
          tabPanel("Half-monthly")
        ),
        fluidRow(
          column(
            width = 12,
            h4("Additional text after tabs")
          )
        )
      )
    )
  )
}


#' subpage1_smoltsize Server Functions
#'
#' @noRd
mod_subpage1_smoltsize_server <- function(id, data, predators_selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$annual_plot <- renderPlot({

   p<- data()%>%
        ggplot(aes(x = length))+
        geom_histogram(aes(color = site, fill = site), alpha= .25) +
        labs( x = "Smolt fork length (mm)",
              y = "Number of smolt",
              color = "Detection site",
              fill = "Detection site")+
        # scale_fill_manual (values = c("steelblue4", "#b47747")) +
        # scale_color_manual(values = c("steelblue4", "#b47747")) +
        theme_light() +
       facet_wrap(~year, ncol = 4, scales = "free_y")+
        theme(strip.text = element_text(color = "black"))


   # Add geom_vlines for selected predators
   if (!is.null(predators_selected())) {
     for (predator in predators_selected()) {
       thresholds <- predator_thresholds %>%
         filter(species == predator)

       # Set colors based on predator
       line_color <- ifelse(predator == "N. Pikeminnow", "darkgreen", "goldenrod")
       dash_color <- ifelse(predator == "N. Pikeminnow", "darkgreen", "goldenrod")

       p <- p +
         geom_vline(
           xintercept = thresholds$median,
           color = line_color,
           linetype = "solid",
           show.legend = TRUE,
           aes(linetype = "Median", color = "Median")
         ) +
         geom_vline(
           xintercept = thresholds$min,
           linetype = "dashed",
           color = dash_color,
           show.legend = TRUE,
           aes(linetype = "Min/Max", color = "Min/Max")
         ) +
         geom_vline(
           xintercept = thresholds$max,
           linetype = "dashed",
           color = dash_color,
           show.legend = FALSE,
           aes(linetype = "Min/Max", color = "Min/Max")
         )
     }
   }
# Add custom legend for color (histogram) and linetype (vlines)
     p <- p +
     scale_fill_manual(
       name = "Detection Site",
       values = c("steelblue4", "#b47747"),
       labels = c("BON", "LWG")
     ) +
     scale_color_manual(
       name = "Predator Species",
       values = c("darkgreen", "goldenrod"),
       labels = c("N. Pikeminnow", "Pacific Hake")
     ) +
     scale_linetype_manual(
       name = "Threshold",
       values = c("solid", "dashed"),
       labels = c("Median", "Min/Max")
     )
p
    })
  })
}


## To be copied in the UI
# mod_subpage1_smoltsize_ui("subpage1_smoltsize_1")

## To be copied in the server
# mod_subpage1_smoltsize_server("subpage1_smoltsize_1")
