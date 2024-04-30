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
        title = "Background information for HydroSurvSizePred shinyAPP,",
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
        HTML("
             <div>
             <p>Implementing Emmett (2008) – Pacific Hake data:</p>
             <ul>
             <li>Emmett (2008), page 5, boxplot image was inserted into
             <a href='https://plotdigitizer.com/app'>https://plotdigitizer.com/app</a>
             and the five-summary data was extracted from 1998-2004, 2017, and 2019 and saved into “Pacific Hake Data.csv”.
             </li>
             <li>Data was then converted from predator length to prey length utilizing y~0.350838x-33.3482 to apply the linear relationship to determine prey size susceptible to predation (Muir et al 2006, page 1526, for Pacific Hake). [Pacific Hake Conversion.R]</li>
             <li>The five-summary data was then graphed utilizing lines of dark green where low & high were dotted lines, q1 & q3 were dashed lines, and median was a solid line.</li>
             </ul>
             <p>Getting Norther Pike threshold:</p>
             <ul>
             <li>For the Northern Pikeminnow length distributions (ranges & medians), we utilized Figure 10 in Winther et al. 2021 and Winther et al. 2019, Table D-1 in Storch et al. 2014, and Figure 10 in Porter et al. 2013.</li>
             <li>For each figure, we obtained the min and max values as well as the median range. From there, we took the median of the median range and used that as the median threshold.</li>
             <li>Median of each bin, weight by the percentage observed, sum across bins => average.</li>
             <li>Since all the graphs had relatively the same shape and similar averages, the thresholds are set static across all years to min = 76.665, median = 166.165, and max = 255.165 after using the Muir equation for N. Pikeminnows.</li>
             </ul>
             <p>Climate years:</p>
             <ul>
             <li>While we were able to get data for predators in a few years to determine their thresholds, a large majority of years did not have such collected data. Thus, using Oceanic Niño Index (ONI) and Pacific Decadal Oscillation (PDO), we categorized both dataset values into negative, neutral, and positive. If a year had positive values from both datasets, the year would be labeled as “positive,” vice versa for “negative.” If the year had differing values, it would be labeled as “neutral.”</li>
             <li>From there, we took the threshold data that was available to us and separated them based on their year’s climate and then averaged them to determine a “default” threshold for each of the three climates.</li>
             </ul>
             </div>
             "
             )
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
