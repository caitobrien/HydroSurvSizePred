#' survival_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_survival_plot <- function(data){

  p<- data %>%
    mutate(reach = factor(reach,
      levels = c("LGR_BON", "BON_BOA", "LGR_BOA"),
      labels = c("LGR_BON", "BON_BOA", "LGR_BOA"))
      ) %>%
    ggplot(aes(x = year, y = median/100))+
    geom_pointrange(aes(color= migration,y=median/100, ymin=min/100, ymax=max/100),shape =21, size = 1, lwd =.25)+

  #  geom_point(data = df_pred_summary, aes(x=as.numeric(year), y=pct_susceptible, shape = predator))+

    labs( y= "Estimated survival (%)",
          x = "Smolt release year",
         # shape = "Predation risk (%)",
          color = "Passage type") +
    scale_y_continuous(labels = scales::percent_format())+
    facet_wrap(~reach, ncol =1,scales = "free")+
    theme_light() +
    theme(panel.spacing.y = unit(1, "cm"))

  plotly::ggplotly(p, height = 900 )

}


