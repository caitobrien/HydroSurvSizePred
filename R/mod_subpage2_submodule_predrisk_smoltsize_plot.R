#' subpage2_submodule_predrisk_smoltsize_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subpage2_submodule_predrisk_smoltsize_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 3,
      "Select predator(s) and year of interest:",

      #select year
      shinyWidgets::pickerInput(
        inputId = ns("select_year"),
        label = "Select year",
        choices = sort(unique(df_fish$year)),
        selected = 2019,
        options = list(`actions-box` = TRUE),
        multiple = FALSE
        # )
      ),

        # select passage type
      shinyWidgets::prettyCheckboxGroup(
          inputId = ns("select_pass"),
          label = "Passage type(s)",
          shape = "curve",
          outline = TRUE,
          choices = unique(df_fish$pass_type_T_R),
          selected = "ROR"
        ),

        #select predator thresholds
        # column(
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("select_predator"),
            label = "Predator threshold(s)",
            shape = "curve",
            outline = TRUE,
            choices = unique(predator_thresholds$species),
            selected = NULL
          )
    ),

    column(
      width = 9,
      plotOutput(ns("histogram")),
      htmlOutput(ns("percentage_text"))
    )
    )
  )
}

#' subpage2_submodule_predrisk_smoltsize_plot Server Functions
#'
#' @noRd
mod_subpage2_submodule_predrisk_smoltsize_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_data <- reactive({
      df_fish %>%
        dplyr::filter(
          site == "LWG", #%in% c(input$select_loc) include if want to add bon sites
          year == input$select_year,
          pass_type_T_R %in% c(input$select_pass)
        )
    })

    output$histogram <- renderPlot({

      selected_pass_types <- input$select_pass

      ## Create the title based on passtypes selected
      # rct.title <- switch(length(selected_pass_types),
      #                     0 =  "Smolt size distribution at LWG (all passage types) below predator size thresholds",
      #                     1 =  "Smolt size distribution at LWG by passage type below predator size thresholds"
      #                     )

        plot <- ggplot2::ggplot(filtered_data(),  ggplot2::aes(x = length)) +
          ggplot2::geom_histogram(binwidth = 5, fill = "white", color = "black", size = 1) +
          ggplot2::labs(  x = "Smolt fork length (mm)",
                          y = "Number of smolt") +
          ggplot2::theme_classic()

        # Conditionally add facet_grid()// change title based on selected passtypes
        if(is.null(selected_pass_types)) {
          plot + ggplot2::ggtitle("Smolt size distribution at LWG (all passage types) below predator size thresholds")
        } else if (length(selected_pass_types) == 1) {
          plot <- plot + ggplot2::facet_grid(~pass_type_T_R) + ggplot2::ggtitle("Smolt size distribution at LWG by passage type below predator size thresholds")
        } else if(length(selected_pass_types) == 2) {
          plot1 <- plot + ggplot2::facet_grid(~pass_type_T_R) + ggplot2::ggtitle( "Smolt size distribution at LWG by passage type below predator size thresholds")
          plot2 <- plot + ggplot2::ggtitle( "Smolt size distribution at LWG (all passage types) below predator size thresholds")

          plot <- plot1/plot2
        }

        if (length(selected_pass_types) == 2) {
          plot1 <- plot
          plot2 <- ggplot2::ggplot(filtered_data(),  ggplot2::aes(x = length)) +
            ggplot2::geom_histogram(binwidth = 5, fill = "white", color = "black", size = 1) +
            # ggplot2::labs(title = "ROR & T smolt size distribution at LWG below predator size thresholds") +
            ggplot2::theme_classic()
        }

        selected_predators <- input$select_predator
        for (predator in selected_predators) {
          threshold <- predator_thresholds$median[predator_thresholds$species == predator]
          if (!is.null(threshold) && any(filtered_data()$length < threshold)) {
            plot <- plot +
              ggplot2::geom_histogram(data = subset(filtered_data(), length < threshold), binwidth = 5, fill = "black", color = "white", alpha=.3)

          }
        }

        plot

    })

    #hoover text
    output$percentage_text <- renderUI({
      selected_predators <- input$select_predator
      selected_year <- input$select_year
      selected_pass_types <- input$select_pass

      percentages <- c()

      if (length(selected_pass_types) > 1) {
        for (predator in selected_predators) {
          threshold <- predator_thresholds$median[predator_thresholds$species == predator]
          if (!is.null(threshold)) {
            filtered_data_both_pass_types <- filtered_data() %>%
              dplyr::filter(length < threshold)
            if (nrow(filtered_data_both_pass_types) > 0) {
              percentage <- round(nrow(filtered_data_both_pass_types) / nrow(filtered_data()) * 100, 2)
              percentages <- c(percentages, paste0("ROR & T : ", predator, " = ", percentage, "%"))
            }
          }
        }
      } else {
        for (pass_type in selected_pass_types) {
          for (predator in selected_predators) {
            threshold <- predator_thresholds$median[predator_thresholds$species == predator]
            if (!is.null(threshold)) {
              filtered_data_pass_type <- filtered_data() %>%
                dplyr::filter(pass_type_T_R == pass_type & length < threshold)
              if (nrow(filtered_data_pass_type) > 0) {
                percentage <- round(nrow(filtered_data_pass_type) / nrow(filtered_data() %>% dplyr::filter(pass_type_T_R == pass_type)) * 100, 2)
                percentages <- c(percentages, paste0( pass_type, " : ",  predator, " = ",  percentage, "%"))
              }
            }
          }
        }
      }

      title <- paste0("For year ", selected_year, ", percent of smolt distribution below predator threshold:")

      if(!is.null(selected_predators)){
        HTML(paste(title, paste(percentages, collapse = "<br>"), sep = "<br>"))
      } else{ NULL }
    })

  })
}



## To be copied in the UI
# mod_subpage2_submodule_predrisk_smoltsize_plot_ui("subpage2_submodule_predrisk_smoltsize_plot_1")

## To be copied in the server
# mod_subpage2_submodule_predrisk_smoltsize_plot_server("subpage2_submodule_predrisk_smoltsize_plot_1")
