#' background_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_background_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Supplementary information for HydroSurvSizePred shinyAPP,",
        em("an exploratory app of size, predation risk, and survival in the hydrosystem")
      )
      ),
    fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = FALSE,
        title = "Salmon Length – Predator Survey Method Documentation",
        status = "info",
        br(),
        shiny::includeHTML(system.file("app/www/mod_background_methods_text.html", package = "HydroSurvSizePred"))

      )
      ),
      fluidRow(
        shinydashboard::box(
          title = "References",
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          shiny::includeHTML(system.file("app/www/mod_background_references_text.html", package = "HydroSurvSizePred"))
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "Additional Information",
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          HTML("All code featured in this Shiny application is made publically available through a GitHub repository: <a href='https://github.com/caitobrien/HydroSurvSizePred.git'><i class='fab fa-github'></i> HyrdroSurvSizePred</a>"
          )
        )
      ),

      fluidRow(
        shinydashboard::box(
          title = "Contact Information",
          status = "info",
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          HTML("<p>This ShinyApp is a product of Columbia Basin Reasearch, School of Aquatic and Fishery Sciences, College of the Environment, University of Washington.</p>
               <p>Please direct general questions to: <a href='mailto:web@cbr.washington.edu'>web@cbr.washington.edu</a></p>"
          )
        )
      )
    )
}

#' background_page Server Functions
#'
#' @noRd
mod_background_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_background_page_ui("background_page_1")

## To be copied in the server
# mod_background_page_server("background_page_1")
