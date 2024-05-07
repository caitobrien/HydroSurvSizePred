#' survival_predrisk_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_survival_predrisk_plot <- function(data_surv, data_pred_risk) {


  # Create a data frame that includes all combinations of year, site, and predator
  complete_data <- expand.grid(year = 1998:2019,
                               site = unique(data_pred_risk$site),
                               predator = unique(data_pred_risk$predator))

  # Join with the original data
  df_pred_summary_complete <- dplyr::left_join(complete_data, data_pred_risk, by = c("year", "site", "predator"))

  # Add a 'site' column to survival data
 data_surv$site <- ifelse(data_surv$migration == "In-river", "BON", "LWG")


    ggplot2::ggplot() +
      ggplot2::geom_bar(data = dplyr::filter(df_pred_summary_complete, dplyr::between(year, 1998,2019)),
                        ggplot2::aes(x = year,
                                     y = pct_susceptible*100,
                                     color = site,  # Color represents site
                                     fill = predator),  # Fill represents predator
                        stat = "identity",
                        position = ggplot2::position_dodge(preserve = "single"),
                        width = .7,
                        size = 1) +
      ggplot2::geom_pointrange(data = dplyr::filter(data_surv, reach == "LGR_BOA" & dplyr::between(year, 1998,2019)),
                               ggplot2::aes(x = year,
                                            y = median*100/8,
                                            ymin = min*100/8,
                                            ymax = max*100/8,
                                            color = site),
                               position =  ggplot2::position_dodge(width = 0.9)) +
      ggplot2::scale_fill_manual(values = c("N. Pikeminnow" = "black","Pacific Hake" = "grey")) +
      ggplot2::scale_color_manual(values = c("BON" = "steelblue4", "LWG" = "#b47747"),
                                  label = c("BON:In-river", "LWG:Transported")) +
      ggplot2::scale_x_continuous(breaks = seq(1998, 2019, by = 1))+
      ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      ggplot2::labs(x = "Year",
                    y = "Percent Susceptible\n[Predation]",
                    fill = "Predation:",
                    color = "Location:Passage Type") +
      ggplot2::scale_y_continuous(
        sec.axis = ggplot2::sec_axis(~.*8/100, name = "Estimated Percent Survival\n[Location:Passage Type]")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = "white"))) +
      ggplot2::theme_minimal()+
      ggplot2::theme(
        panel.spacing.x =  ggplot2::unit(.25, "lines"),
        legend.position = "top",
        panel.grid.minor  = ggplot2::element_blank(),
        axis.text.y.left = ggplot2::element_text(color = "black"),
        text = ggplot2::element_text(size = 15),
        legend.title = ggplot2::element_text(face = "bold")
        )

}
