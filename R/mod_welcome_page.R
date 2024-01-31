#' welcome_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Welcome to the HydroSurvSizePred shinyAPP,",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        em("an exploratory tool...")
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinydashboard::box(
          title = "What does this application do?",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          div(
            HTML("<p>This application can be used to explore spring/summer Chinook salmon, <em>Oncorhynchus tshawytscha</em>,  smolt length distributions, predator risk from Northern Pikeminnow and Pacific Hake, and survival across years and seasons during downstream migration through the Federal Columbia River Power System Hydrosystem (FCRPS)(Figure 1).</p>
                 <p>Please note that this Shiny App is dependent on data availability and serves as an exploratory tool.
                 </p>")
          )
        ),
        shinydashboard::box(
          title = "How to use this application?",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
            HTML("
                 <div style='text-align: left;'>
                  <p>This <b>HydroSurvSizePred</b> is an interactive query tool to explore patterns of spring/summer-run Chinook Salmon passing Lower Granite Dam and Bonneville Dam as smolts and compare the following:
                 </p>
                    <div style='text-align: center; margin: auto;'>
                    <ul style='display: inline-block; text-align: left;'>
                     <li><b>Smolt length:</b> The distribution of smolt lengths can be explored across years, months, and half-months throughout the outmigration season. These data were queried from PTAGIS.</li>
                     <li><b>Predation risk:</b> The percentages of smolts below size thresholds susceptible to fish predation can be viewed. The proportions were calculated following Muir et al. (2006) and the size distributions of N. Pikeminnow (Winther et al. 2021) and Pacific Hake (Taylor et al. 205) observed each year.</li>
                     <li><b>Survival:</b> Estimates of survival from LWG-BON (downstream survival), BON-BOA (estuary & ocean survival), and LWG-BOA were determined from PIT tagged fish data from PTAGIS. Survival of transported fish from LGR-BON is assumed to be 1.</li>
                     <li><b>Length, predation risk, survival:</b> A comparison between length distributions, predation risk, and LWG-BOA survival is available for viewing across years.
                     </li>
                 </ul>
                 </div>
                 </div>")
      )
      ),
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          shiny::img(src = "www/map.png", style = "max-width:100%; height:auto;"),
          br(),
          "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles) along Spring/Summer Chinook salmon and Steelhead migratory routes."
        )
      )
    )
  )
}

#' welcome_page Server Functions
#'
#' @noRd
mod_welcome_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
