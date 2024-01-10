#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(ggplot2)
library(dplyr)
library(shinyWidgets)

# Generating sample data
set.seed(123)
data <- data.frame(
  year = rep(2000:2004,20),
  prey_size = rnorm(100, mean = 50, sd = 10),
  predator1_size = rnorm(100, mean = 40, sd = 8),
  predator2_size = rnorm(100, mean = 45, sd = 6)
)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),


     # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Size-selective Predator Survival Predictions"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          menuItem("Welcome", tabName = "welcome", icon = icon("home")),
          menuItem("SizePred Survival App", tabName = "figs", icon = icon("chart-line"),
                   menuSubItem("Size, Predation, and Survival", tabName = "mainpage", icon = icon("diagram-project")),
                   menuSubItem("Smolt Size", tabName = "subpage1", icon = icon("ruler-horizontal")),
                   menuSubItem("Predation Risk", tabName = "subpage2", icon = icon("arrow-trend-down")),
                   menuSubItem("Estimated Survival", tabName = "subpage3", icon = icon("arrow-trend-up"))
          ),
          menuItem("Background", tabName = "background", icon = icon("info"))
        )
      ),
      shinydashboard::dashboardBody(
        tabItems(
          tabItem("welcome", h2("Welcome to Columbia River Fish Survival Dashboard")),
          tabItem("mainpage", h2("Summary of smolt size, predation risk and estimated survival for Chinook Salmon"),
                  mod_mainpage_ui("mainpage_1")),
          tabItem("subpage1", h2("Smolt Size Content")),
          tabItem("subpage2", h2("Predation Risk Content")),
          tabItem("subpage3", h2("Estimated Survival Content")),
          tabItem("background", h2("Background Information"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HydroSurvSizePred"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
