library(shiny)
library(dplyr)
library(ggplot2)



# Generating sample data
ui <- fluidPage(

  selectInput(
    inputId = "select_loc",
    label = "Locations",
    choices = unique(df$site),
    selected = unique(df$site),
    # width = "200px",
    multiple = T
  ),

  sliderInput(
    inputId = "year_slider",
    label = "Select Year",
    min = min(df$year),
    max = max(df$year),
    value = c(min(df$year), max(df$year)),
    step = 1,
    sep = ""
  ),
  plotOutput(outputId = "filtered_plot")
)

server <- function(input, output, session) {
  # Reactive expression for filtered data
  filtered_data <- reactive({
    df %>%
      filter(
        year == c(input$year_slider[1]:input$year_slider[2]),
        site %in% c(input$select_loc))
  })

  # Render plot
  output$filtered_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = site, y = length)) +
      geom_point() +
      labs(title = "Filtered Data") +
      facet_wrap( ~ year)
  })
}

shinyApp(ui, server)
