#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(ggh4x)
library(patchwork)

# Generating sample data
set.seed(123)
data.test <- expand.grid(
  year = 2000:2005,
  doy = 150:200,
  site = c("BON", "LWG"),
  pass_type = c("T", "R")
) %>%
  mutate(
    length = rep( rnorm(165, mean = 120, sd = 10), length.out = 1224),
    pctsurv = rep(rnorm(100, mean = 30, sd = 15), length.out = 1224),
    pctprey = rep(rnorm(100, mean = 50, sd =20), length.out = 1224),
    predator = rep(c("PH", "NH"), length.out = 1224),
    date = as.Date(paste(year,doy), format = "%Y %j"))

predator_thresholds<-tibble(species = c("N. Pikeminnow", "Pacific Hake"),
                            median = c(166.165, 110),
                            min = c(76.665, 70),
                            max = c(255.165, 200))

pred_long<-predator_thresholds %>%
  pivot_longer(cols = c(median, min, max), values_to = "threshold", names_to = "type") %>%
  mutate(type = ifelse(type != "median","min/max", "median"),
         predator = species)


df_fish<-read.csv(here::here("data/size_distribution", "spsuCH_subset.csv"))

df_pred_summary<-read.csv(here::here("data/predation_risk", "predtopreySL_to_pctsusceptible.csv"))

df_survival<-read.csv(here::here("data/survival", "cjs_reach_survival.csv"))


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
          menuItem("Welcome", tabName = "welcome_page", icon = icon("home")),
          menuItem("SizePred Survival App", tabName = "figs", icon = icon("chart-line"),
                   menuSubItem("Size, Predation, and Survival", tabName = "main_page", icon = icon("diagram-project")),
                   menuSubItem("Smolt Size", tabName = "subpage1", icon = icon("ruler-horizontal")),
                   menuSubItem("Predation Risk", tabName = "subpage2", icon = icon("arrow-trend-down")),
                   menuSubItem("Estimated Survival", tabName = "subpage3", icon = icon("arrow-trend-up")),
                   menuSubItem("Size, Predation, and Survival", tabName = "subpage4", icon = icon("diagram-project"))
          ),
          menuItem("Background", tabName = "background_page", icon = icon("info"))
        )
      ),
      shinydashboard::dashboardBody(
        tabItems(
          tabItem("welcome_page",
                  mod_welcome_page_ui("welcome_page_1")),
          tabItem("main_page", h2("Summary of smolt size, predation risk and estimated survival for Chinook Salmon"),
                  mod_main_page_ui("main_page_1")),
          tabItem("subpage1",
                  mod_subpage1_smoltsize_ui("subpage1_smoltsize_1")),
          tabItem("subpage2", h2("Predation Risk Content"),
                   mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")),
          tabItem("subpage3",
                  mod_subpage3_survival_ui("subpage3_survival_1")),
          tabItem("subpage4",
                   mod_subpage4_summary_ui("subpage4_summary_1")),
          tabItem("background_page",
                  mod_background_page_ui("background_page_1"))
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
