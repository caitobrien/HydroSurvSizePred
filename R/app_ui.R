#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


#figure a way to make this redundant (with module) or live in data
predator_thresholds<-data.frame(species = c("N. Pikeminnow", "Pacific Hake"),
                            median = c(166.165, 110),
                            min = c(76.665, 70),
                            max = c(255.165, 200))

pred_long<-predator_thresholds %>%
  tidyr::pivot_longer(cols = c(median, min, max), values_to = "threshold", names_to = "type") %>%
  dplyr::mutate(type = ifelse(type != "median","min/max", "median"),
         predator = species)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),


    shinydashboard::dashboardPage(
      #header
      shinydashboard::dashboardHeader(title = "Columbia Basin Research"),

      #side panel
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem("About", tabName = "about_page", icon = icon("home")),
          shinydashboard::menuItem("SizePred Survival App", tabName = "figs", icon = icon("chart-line"),
                                   shinydashboard::menuSubItem("Size, Predation, and Survival", tabName = "subpage4", icon = icon("diagram-project"), selected = TRUE),
                                   shinydashboard::menuSubItem("Smolt Size", tabName = "subpage1", icon = icon("ruler-horizontal")),
                                   shinydashboard::menuSubItem("Predation Risk", tabName = "subpage2", icon = icon("arrow-trend-down")),
                                   shinydashboard::menuSubItem("Estimated Survival", tabName = "subpage3", icon = icon("arrow-trend-up"))
                                   ),
          shinydashboard::menuItem("Supplementary Information", tabName = "background_page", icon = icon("info"))
        )
      ),

      #body
      shinydashboard::dashboardBody(

        #adjust default shinydashboard theme colors
        fresh::use_theme(CBRtheme),

        #submenu items
        shinydashboard::tabItems(
          shinydashboard::tabItem("about_page",
                  mod_about_page_ui("about_page_1")),
          shinydashboard::tabItem("subpage1",
                  mod_subpage1_smoltsize_ui("subpage1_smoltsize_1")),
          shinydashboard::tabItem("subpage2",
                   mod_subpage2_predrisk_ui("mod_subpage2_predrisk_1")),
          shinydashboard::tabItem("subpage3",
                  mod_subpage3_survival_ui("subpage3_survival_1")),
          shinydashboard::tabItem("subpage4",
                   mod_subpage4_summary_ui("subpage4_summary_1")),
          shinydashboard::tabItem("background_page",
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
