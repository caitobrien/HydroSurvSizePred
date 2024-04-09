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

    #header image
    fluidRow(
      div(
        tags$img(src="www/welcomebanner_SIZEPRED.svg",  style = "width: 100%; height: auto; max-height: 400px; object-fit: cover;"), #adjust image& text in AI --save as .svg
        br(),
        br()
      )
    ),


    #Info box
    fluidRow(

        shinydashboard::box(
          title = "What does this application do?",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          status = "primary",
          shiny::includeHTML(system.file("app/www/mod_welcome_Q1_text.html", package = "HydroSurvSizePred")),
        )
        ),

    #overview
    fluidRow(
    shinydashboard::box(
      title = "Overview",
      width = 12,
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "info",
      br(),
      HTML("<p>Smolts migrating downstream the Snake and Columbia rivers towards the Pacific Ocean encounter fish predators, such as Northern Pikeminnow (N. Pikeminnow, <em>Ptychocheilus oregonensis</em>). As the smolts migrate through the early ocean environment, they encounter additional fish predators, such as Pacific Hake,<em> Merluccius productus</em> (Muir et al. 2006). Size-selective predation can be particularly important for small salmon smolts. Their sizes differ across years and through the season, depending on conditions experienced that affect their growth.</p>
            <p>Smolts may also differ in size depending on the type of passage: in-river or transported passage through the hydrosystem (Lower Granite Dam [LWG] to Bonneville Dam [BON]) (Figure 1). Transported smolts may be smaller than in-river smolts because of loss of growth opportunity during their short passage through the hydrosystem (transported: 2 days; in-river: ~2-4 weeks). Smaller smolts will be at greater risk of predation than larger smolts, and predation risk will also depend on the size distribution of the predators. The proportion of fish below a size threshold can be an indicator of survival (Muir et al. 2006).</p>
              "),
      br(),
      #leaflet map
      fluidRow(
        column(width = 3),  # Empty column to center map
        column(
          width = 6,

          mod_welcome_page_submodule_leaflet_map_ui("welcome_page_submodule_leaflet_map_1"),

          column(width = 3) # Empty column to center map
        )
      ),
      br(),
      HTML("<p>Survival can be estimated through the hydrosystem (LGR-BON), post-hydrosystem (juveniles at BON to adults at BOA), and these reaches altogether (LGR-BOA) from fish tagged with passive integrated transponder tags and through mark-recapture modeling (Cormack 1964, Jolly 1965, Seber 1965, Gosselin et al. 2021).</p>
              <p>This application aims to allow users to view size distribution of smolt and how that may impact predation risk through downstream migration. A user can then compare size distribution and predation risk, to predicted survival based on reach and passage type. To view each factor, see the subpages within the <b>SizePred Survival App</b>, or select the <em>Size, Predation, and Survival</em> subpage to compare all three for specific years of interest.</p>
              <br></br>
              <p><em>Please note that this Shiny App is dependent on data availability and serves as an exploratory tool.</em>
              </p>")


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

    mod_welcome_page_submodule_leaflet_map_server("welcome_page_submodule_leaflet_map_1")

  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
