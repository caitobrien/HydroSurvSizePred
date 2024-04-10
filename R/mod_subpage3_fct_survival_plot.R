#' survival_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_survival_plot <- function(data){

  p<- data %>%
    dplyr::mutate(reach = factor(reach,
      levels = c("LGR_BON", "BON_BOA", "LGR_BOA"),
      labels = c("LGR_BON", "BON_BOA", "LGR_BOA"))
      ) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = median/100))+
    ggplot2::geom_pointrange(ggplot2::aes(color= migration,y=median/100, ymin=min/100, ymax=max/100),shape =21, size = 1, lwd =.25)+

  #  geom_point(data = df_pred_summary, aes(x=as.numeric(year), y=pct_susceptible, shape = predator))+

    ggplot2::labs( y= "Estimated survival (%)",
          x = "Smolt release year",
         # shape = "Predation risk (%)",
          color = "Passage type") +
    ggplot2::scale_y_continuous(labels = scales::percent_format())+
    ggplot2::facet_wrap(~reach, ncol =1,scales = "free")+
    ggplot2::theme_light() +
    ggplot2::theme(panel.spacing.y = ggplot2::unit(1, "cm"),
                   panel.grid.minor = ggplot2::element_blank())

  plotly::ggplotly(p)

}


