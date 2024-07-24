#' about_page_submodule_leaflet_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_page_submodule_leaflet_map_ui <- function(id){
  ns <- NS(id)
  tagList(

      leaflet::leafletOutput(ns("map"),
                             height = "350px"),
      br(),
      div(style = "text-align: left;", "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles)
          along spring/summer Chinook salmon migratory pathway (grey lines: outmigration of in-river (solid) and barge transported (dashed) juveniles; black line: adult return migration).
          HydroSurvSizePred uses data from passive integrated transponder tagged fish detected at select sites (red circles). Predators icons denote Northern Pikeminnow and Pacific Hake.
          "

          )
  )
}

#' about_page_submodule_leaflet_map Server Functions
#'
#' @noRd
mod_about_page_submodule_leaflet_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #dam sites and transportation filter
    rivers_data <-data.frame(
      Name = c("Lower Granite\n(LGR)","Adult returns","Transport", "Little Goose", "Lower Monumental", "Ice Harbor", "McNary", "John Day", "The Dalles", "Bonneville\n(BON)", "Outmigrating detections", "Transport", "adult return"),
      Lat = c(46.66053259552463,46.66053259552463,46.67080357578456, 46.587531484954575, 46.56343049721032, 46.249736092987355, 45.93405510635421,45.71499679473759, 45.614135364912876, 45.64441868254798, 45.64441868254798, 45.652536297369814, 45.64441868254798),
      Lon = c(-117.42737011713677,-117.42737011713677,-117.42019252542396 , -118.0260163871114,-118.54315348896209, -118.87984211781144,  -119.29757721597544,-120.69368083132734,-121.13417428900306,  -121.94060471598787, -121.94060471598787, -121.94060202780263, -121.94060471598787),
      passtype = c(0,0,1,0,0,0,0,0,0,0,0,1,0),
      direction = c(0,2,0,0,0,0,0,0,0,0,1,1,2),
      detection = c(0,0,1,0,0,0,0,0,0,0,1,1,1)
    )

    #icons
    npikeminnow_icon <- leaflet::makeIcon(
      iconUrl = system.file("app/www/Npikeminnow.svg", package = "HydroSurvSizePred"),
      iconWidth = 80, iconHeight = 40
    )
    phake_icon <- leaflet::makeIcon(
      iconUrl = system.file("app/www/Phake.svg", package = "HydroSurvSizePred"),
      iconWidth = 80, iconHeight = 40
    )


    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::setView(lng = -123, lat = 46, zoom = 6) %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = 'Tiles &copy; <a href="https://www.carto.com/">Carto</a>') %>%
        leaflet::addMarkers(
          lng=-124.8, lat=46.6,
          icon = phake_icon,
          popup = "Pacific Hake",

        ) %>%
        leaflet::addMarkers(
          lng=-121, lat=46.5,
          icon = npikeminnow_icon,
          popup = "Northern Pikeminnow",

        ) %>%

        #outgoing- in-river with
        #highlight detection sites for outgoing
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, direction %in% c("1", "0") & detection == 1),
          lng = ~Lon,
          lat = ~Lat+.1,
          label = ~Name,
          color = 'darkred',
          fill = TRUE,
          fillOpacity = 1,
          radius = 5
        ) %>%
        # #outgoing - in-river
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("1", "0")), lng = ~Lon, lat = ~Lat+.1, color = "grey") %>%
        # outgoing - transport
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 1), lng = ~Lon, lat = ~Lat+.1, color = "grey", dashArray = "5, 5") %>%
        #add specific labels for icons (outgoing)
        ##LGR
        leaflet::addAwesomeMarkers(
          lng = -117.42737011713677,
          lat = 46.66053259552463+.1,
          label = HTML("<div style='background-color: lightgrey;width: 200px; white-space: normal;'><b>Juveniles entering the hydrosystem</b><br>
                 All spring/summer Chinook salmon originate upstream of LGR,
                 with a portion transported via barges from LGR to a release site below BON,
                expediting travel time through the hydrosystem, possibly affect later life stages."),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-left", # change icons to one that works!
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        ##BON
        leaflet::addAwesomeMarkers(
          lng = -121.94060471598787,
          lat = 45.64441868254798+.1,
          label = HTML("<div style='background-color: lightgrey; width: 200px; white-space: normal;'><b>Smolt size distribution</b><br>
                       Smolt sizes vary within seasons and across years and may also differ based on the type of passage through the hydrosystem.
                       Transported smolts tend to be smaller due to shorter passage time than their in-river counterparts.
                       Smaller sizes increase predation risk to river and marine predators such as the Northern Pikeminnow and Pacific Hake.
                  </div>"
          ),


          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE, backgroundColor = "lightgrey"),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-left", # change icons to one that works!
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        #base dam sites
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("1", "0") & detection == 0),
          lng = ~Lon,
          lat = ~Lat,
          label = ~Name,
          color = 'darkgrey',
          fill = TRUE,
          fillColor = "black",
          fillOpacity = 1,
          radius = 4
        ) %>%
        #highlight detection sites
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, passtype == 0 & direction == 2 & detection == 1),
          lng = ~Lon,
          lat = ~Lat-.1,
          label = ~Name,
          color = 'darkred',
          fill = TRUE,
          fillOpacity = 1,
          radius = 5
        ) %>%
        #add specific labels for icons (outgoing)
        ##LGR
        leaflet::addAwesomeMarkers(
          lng = -117.42737011713677,
          lat = 46.66053259552463-.1,
          label = HTML("<div style='width: 200px; white-space: normal;'><b>Estimated survival by reach</b>
                       <br>To better understand survival through the hydrosystem, this application highlights predicted survival
                       from LWG-BON (downstream survival), BON-BOA (estuary & ocean survival), and LWG-BOA using CJS modeling.
                       </div>"
          ),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-right", # change icons to one that works!
            iconColor = "black",
            markerColor = 'white'
          )) %>%
        ##BON
        leaflet::addAwesomeMarkers(
          lng = -121.94060471598787,
          lat = 45.64441868254798-.1,
          label = HTML("<div style='width: 200px; white-space: normal;'><b>Predation Risk</b><br>
                Predation risk can vary based on smolt size as well as the the prey size threshold per predator type (Northern Pikeminnow; riverine/estuary habitat, Pacific Hake: ocean habitat). As smolt size changes through salmon migration, so does the risk to predation."),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-right", # change icons to one that works!
            iconColor = "black",
            markerColor = 'white'
          )) %>%
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("2", "0")), lng = ~Lon, lat = ~Lat-.10, color = "black") %>%
        #add crosshairs to reset to setView position
        leaflet::addEasyButton(leaflet::easyButton(
          icon="fa-crosshairs", title="Locate",  onClick = leaflet::JS("function(btn, map) { map.setView([45, -120], 5); }")))
    })

  })
}

## To be copied in the UI
# mod_about_page_submodule_leaflet_map_ui("about_page_submodule_leaflet_map_1")

## To be copied in the server
# mod_about_page_submodule_leaflet_map_server("about_page_submodule_leaflet_map_1")
