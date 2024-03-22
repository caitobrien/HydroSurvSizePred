#' SacPAStheme
#'
#' @description A utils function setting adjusting the default shinydashboard theme
#'
#' @return The return value, will reflect the sacpas website color theme
#'
#' @noRd

SacPAStheme<- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#556b2f",
    aqua = "#556b2f"
  ),
  fresh::adminlte_sidebar(
  dark_bg = "#F8F8F8"
 # dark_hover_bg = "darkgrey" none of the colors seem to do anything other than bg//switch to css option for more piecemeal customization
  ),
  fresh::adminlte_global(
    content_bg = "#FFF",
    box_bg = "#F8F8F8",
    info_box_bg = "#F8F8F8"
  )
)

#search_vars_adminlte2() use for variables to change
