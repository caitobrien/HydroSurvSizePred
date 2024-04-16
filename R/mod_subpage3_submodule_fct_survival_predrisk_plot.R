#' survival_predrisk_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_survival_predrisk_plot <- function(data_surv, data_pred_risk) {

  # Add a 'site' column to df_survival
  data_surv$site <- ifelse(data_surv$migration == "In-river", "BON", "LWG")

  ggplot2::ggplot() +
    ggplot2::geom_bar(data = dplyr::filter(data_pred_risk, dplyr::between(year, 1998,2019)),
                      ggplot2::aes(x = site,
                                   y = pct_susceptible*100,
                                   color = predator,
                                   fill = predator),
                      stat = "identity",
                      position = ggplot2::position_dodge(preserve = "single"),
                      width = .7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::geom_pointrange(data = dplyr::filter(data_surv, reach == "LGR_BOA" & dplyr::between(year, 1998,2019)),
                             ggplot2::aes(x = site, y = median*100/8, ymin = min*100/8, ymax = max*100/8, color = migration),
                             position =  ggplot2::position_dodge(width = 0.9)) +
    ggplot2::labs(x = "Site by Year",
                  y = "Percent Susceptible [Predation Risk]",
                  fill = "Predation",
                  color = "Passage Type") +
    ggplot2::scale_fill_manual(values = c("N. Pikeminnow" = "black","Pacific Hake" = "grey")) +
    ggplot2::scale_color_manual(values = c("In-river" = "steelblue4", "Transported" = "#b47747")) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~.*8/100, name = "Estimated Percent Survival [Passage Type]")) +
    ggh4x::facet_nested(. ~ year + site, scales = "free_x", space = "free_x", switch = "x", nest_line = ggplot2::element_line(linetype = 1)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x =  ggplot2::element_blank(),
                   axis.ticks.x =  ggplot2::element_blank(),
                   axis.line.x =  ggplot2::element_blank(),
                   panel.spacing.x =  ggplot2::unit(.25, "lines"),
                   legend.position = "top",
                   panel.grid = ggplot2::element_blank(),
                   axis.text.y.left = ggplot2::element_text(color = "black")
    )

}
