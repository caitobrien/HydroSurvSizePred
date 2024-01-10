#' mainpage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mainpage_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        checkboxGroupInput(inputId = ns("predator_selector"),
                           label = "Select Predator(s):",
                           choices = c("Predator 1", "Predator 2")),
        actionButton(inputId = ns("button_reset"),
                     label = "Reset Selections"),
        actionButton(inputId = ns("button_update"),
                     label = "Update"),
          # prompt to select all years or by year
          selectInput(
            inputId = ns("year_display"),
            label = "View by",
            choices = c("All Years", "Year"),
            selected = "All Years"
          ),
          uiOutput(ns("year_picker"))

        )
      ),

      box(
        title = "Smolt size distribution and predation risk",
        width = 12,
        plotOutput(outputId = ns("histogram")),
        br(),
        verbatimTextOutput(outputId = ns("percentage_below_threshold")),
        br(),
        verbatimTextOutput(outputId = ns("percentage_below_per_predator"))
      ),
      box(
        title = "Survival Data",
        width = 12

      )
    )
}

#' mainpage Server Functions
#'
#' @noRd
mod_mainpage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Render the UI for the year picker
    output$year_picker <- renderUI({
      ns <- session$ns

      if (input$year_display == "Year") {
        pickerInput(
          inputId = ns("select_year"),
          label = "Select Year",
          choices = unique(data$year),
          selected = 2000,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      } else {
        NULL
      }
    })

    filtered_data <- reactive({
      if (input$year_display == "All Years") {
        data
      } else if (input$year_display == "Year" && !is.null(input$select_year)) {
         data %>%
          filter(year == input$select_year)
          group_by(year) %>%
          summarize_all(list(mean = mean))
      } else {
        NULL
      }
    })

    thresholds <- reactiveValues(threshold_predator1 = NULL, threshold_predator2 = NULL)

    observeEvent(input$button_update, {
      selected_predators <- input$predator_selector

      if ("Predator 1" %in% selected_predators) {
        thresholds$threshold_predator1 <- 40 # Set your desired threshold for Predator 1
      } else {
        thresholds$threshold_predator1 <- NULL
      }

      if ("Predator 2" %in% selected_predators) {
        thresholds$threshold_predator2 <- 60 # Set your desired threshold for Predator 2
      } else {
        thresholds$threshold_predator2 <- NULL
      }
    })

    output$histogram <- renderPlot({
      tryCatch({
        threshold_predator1 <- thresholds$threshold_predator1
        threshold_predator2 <- thresholds$threshold_predator2

        # Create a histogram
        plot <- ggplot(data, aes(x = prey_size)) +
          geom_histogram(binwidth = 5, fill = "grey", color = "black") +
          labs(title = "Histogram with Predator Size Thresholds") +
          theme_minimal()

        # Highlight bars below each predator size threshold
        if (!is.null(threshold_predator1) && any(data$prey_size < threshold_predator1)) {
          plot <- plot +
            geom_histogram(data = subset(data, prey_size < threshold_predator1), binwidth = 5, fill = "sienna3", color = "black", alpha=.3) +
            geom_text(data = subset(data, prey_size < threshold_predator1),
                      aes(x = threshold_predator1 - 10, y = 10, label = paste0(round(sum(prey_size < threshold_predator1) / nrow(data) * 100, 2), "%")),
                      color = "sienna3", size = 4)
        }

        if (!is.null(threshold_predator2) && any(data$prey_size < threshold_predator2)) {
          plot <- plot +
            geom_histogram(data = subset(data, prey_size < threshold_predator2), binwidth = 5, fill = "sienna", color = "black", alpha = .3) +
            geom_text(data = subset(data, prey_size < threshold_predator2),
                      aes(x = threshold_predator2 - 10, y = 10, label = paste0(round(sum(prey_size < threshold_predator2) / nrow(data) * 100, 2), "%")),
                      color = "sienna", size = 4)
        }

        plot  # Return the plot object
      }, error = function(e) {
        cat("Error in renderPlot:", conditionMessage(e), "\n")
        print(e)
        return(NULL)
      })
    })

    output$percentage_below_threshold <- renderText({
      total_count <- nrow(data)
      count_below_threshold <- 0

      if (!is.null(thresholds$threshold_predator1)) {
        count_below_threshold <- count_below_threshold + sum(data$prey_size < thresholds$threshold_predator1)
      }
      if (!is.null(thresholds$threshold_predator2)) {
        count_below_threshold <- count_below_threshold + sum(data$prey_size < thresholds$threshold_predator2)
      }

      percentage_below <- (count_below_threshold / total_count) * 100
      paste("Total % Below Threshold:", round(percentage_below, 2), "%")
    })

    output$percentage_below_per_predator <- renderText({
      count_below_threshold_pred1 <- ifelse(!is.null(thresholds$threshold_predator1), sum(data$prey_size < thresholds$threshold_predator1), 0)
      count_below_threshold_pred2 <- ifelse(!is.null(thresholds$threshold_predator2), sum(data$prey_size < thresholds$threshold_predator2), 0)

      total_count <- nrow(data)
      percentage_below_pred1 <- (count_below_threshold_pred1 / total_count) * 100
      percentage_below_pred2 <- (count_below_threshold_pred2 / total_count) * 100

      paste("Predator 1 % Below Threshold:", round(percentage_below_pred1, 2), "%\n",
            "Predator 2 % Below Threshold:", round(percentage_below_pred2, 2), "%")
    })

    observeEvent(input$button_reset, {
      updateCheckboxGroupInput(session, "predator_selector", selected = character(0))
      thresholds$threshold_predator1 <- NULL
      thresholds$threshold_predator2 <- NULL
    })
  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
