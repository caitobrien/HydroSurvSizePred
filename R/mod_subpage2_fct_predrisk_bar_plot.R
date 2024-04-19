#' predrisk_bar_plot
#'
#' @description function for predation risk bar plots
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_predrisk_bar_plot <- function(data){

  # p<- data %>%
  #   dplyr::mutate(year = as.factor(year)) %>%
  #   ggplot2::ggplot(ggplot2::aes(x = year, y = pct_susceptible, fill = site))+
  #   ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(preserve = "single"), width = .8) +
  #   ggplot2::scale_fill_manual(values = c("steelblue4", "#b47747") )+
  #   ggplot2::labs( y= "Percent susceptible to predation",
  #         x = "Smolt release year",
  #         fill = "Location") +
  #   ggplot2::scale_y_continuous(labels = scales::percent_format())+
  #   ggplot2::facet_wrap(~predator, ncol =1)+
  #   ggplot2::theme_light() +
  #   ggplot2::theme(panel.spacing.y = ggplot2::unit(1, "cm"))
  # plotly::ggplotly(p, height = 700 )



    # p<- data %>%
    #   dplyr::mutate(Year = as.factor(year),
    #                 Location = site,
    #                 `Percent Susceptible` = round(pct_susceptible*100)) %>%
    #   ggplot2::ggplot(ggplot2::aes(x = Year,
    #                                y = `Percent Susceptible`,
    #                                fill = Location)
    #                   )+
    #   ggplot2::geom_bar(stat = "identity",
    #                     position = ggplot2::position_dodge(preserve = "single"),
    #                     width = .8) +
    #   ggplot2::scale_fill_manual(values = c("steelblue4", "#b47747") )+
    #   ggplot2::labs( y= "Percent susceptible to predation",
    #                  x = "Smolt release year",
    #                  fill = "Location") +
    #   # ggplot2::scale_y_continuous(labels = scales::percent_format())+
    #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))+
    #   ggplot2::facet_wrap(~predator,
    #                       ncol = 1)+
    #   ggplot2::theme_light() +
    #   ggplot2::theme(panel.spacing.y = ggplot2::unit(1, "cm"),
    #                  panel.grid = ggplot2::element_blank())

  data <- data %>%
    dplyr::mutate(Year = as.factor(year),
                  Location = site,
                  `Percent Susceptible` = round(pct_susceptible*100,2))

  p <- plotly::plot_ly(data, x = ~Year, y = ~`Percent Susceptible`, color = ~Location, colors = c( "#b47747", "steelblue4"), type = "bar") %>%
    plotly::layout(yaxis = list(title = "Percent susceptible to predation", tickformat = ".2f", tickprefix = "", ticksuffix = "%"),
           xaxis = list(title = "Smolt release year"),
           barmode = 'group')

  p  <- plotly::config(p,
                       displayModeBar = TRUE,
                       modeBarButtonsToRemove = list(
                         "zoom2d",
                         "autoScale2d",
                         "lasso2d",
                        "pan2d",
                        "select2d",
                        "zoomIn2d",
                        "zoomOut2d",
                        "autoScale2d",
                        "resetScale2d",
                        "hoverClosestCartesian",
                        "hoverCompareCartesian"
                       )
  )

}
