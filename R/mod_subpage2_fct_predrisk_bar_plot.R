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



    p<- data %>%
      dplyr::mutate(Year = as.factor(year),
                    Location = site,
                    `Percent Susceptible` = round(pct_susceptible*100)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year,
                                   y = `Percent Susceptible`,
                                   fill = Location)
                      )+
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(preserve = "single"),
                        width = .8) +
      ggplot2::scale_fill_manual(values = c("steelblue4", "#b47747") )+
      ggplot2::labs( y= "Percent susceptible to predation",
                     x = "Smolt release year",
                     fill = "Location") +
      # ggplot2::scale_y_continuous(labels = scales::percent_format())+
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))+
      ggplot2::facet_wrap(~predator,
                          ncol = 1)+
      ggplot2::theme_light() +
      ggplot2::theme(panel.spacing.y = ggplot2::unit(1, "cm"),
                     panel.grid = element_blank())

    plotly::ggplotly(p)

}
