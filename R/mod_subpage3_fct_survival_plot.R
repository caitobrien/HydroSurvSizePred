#' survival_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_survival_plot <- function(data) {
  filtered_data <- data %>%
    dplyr::mutate(reach = factor(reach,
      levels = c("LGR_BON", "BON_BOA", "LGR_BOA"),
      labels = c("LGR_BON", "BON_BOA", "LGR_BOA")
    )) %>%
    dplyr::mutate(
      hovertext = paste("Year: ", year,
                        "<br>Passage type: ", migration,
                        "<br>Estimated survival, median: ", scales::percent(median/100, accuracy = 0.01),
                        "<br>95% CI upper: ", scales::percent(max/100, accuracy = 0.01),
                        "<br>95% CI lower: ", scales::percent(min/100, accuracy = 0.01)))

  p <- ggplot2::ggplot(filtered_data, ggplot2::aes(
    x = year,
    y = median,
    text = hovertext
  )) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        color = migration,
        y = median,
        ymin = min,
        ymax = max
      ),
      shape = 21,
      size = 1,
      lwd = .25
    ) +
    ggplot2::labs(
      y = "Estimated survival (%)",
      x = "Smolt release year",
      color = "Passage type"
    ) +
    # ggplot2::scale_color_manual(values = c("In-river" = "steelblue4", "Transported" = "#b47747")) +
   ggplot2::facet_wrap(~reach, ncol = 1, scales = "free") +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.spacing.y = ggplot2::unit(1, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      text = ggplot2::element_text(size = 15)
    )

  p <- plotly::ggplotly(p, tooltip = "text")

  p <- plotly::config(p,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = list(
      "zoom2d",
      "autoScale2d",
      "lasso2d",
      "pan2d",
      #                  "select2d",
      # "zoomIn2d",
      "zoomOut2d",
      "autoScale2d"
      #                  "resetScale2d",
      #                  "hoverClosestCartesian",
      #                  "hoverCompareCartesian"
    )
  )

  p
}
