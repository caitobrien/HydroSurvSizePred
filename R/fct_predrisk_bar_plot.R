#' predrisk_bar_plot
#'
#' @description function for predation risk bar plots
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_predrisk_bar_plot <- function(data){

  p<- data %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(x = year, y = pct_susceptible, fill = site))+
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = .8) +
    scale_fill_manual(values = c("steelblue4", "#b47747") )+
    labs( y= "Percent susceptible to predation",
          x = "Smolt release year",
          fill = "Location") +
    scale_y_continuous(labels = scales::percent_format())+
    facet_wrap(~predator, ncol =1)+
    theme_light() +
    theme(panel.spacing.y = unit(1, "cm"))
  plotly::ggplotly(p, height = 700 )
}
