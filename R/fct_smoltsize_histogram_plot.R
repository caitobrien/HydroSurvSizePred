#' smoltsize_histogram
#'
#' @description A function used to produce the smolt size histogram
#' @return The return value is a histogram allows for variable facets (by_year, by_month, by_half_month)
#'
#'
#' @noRd

fct_smoltsize_histogram_plot <- function(data, facet_by){
  p <- data %>%
    ggplot(aes(x = length)) +
    geom_histogram(aes(color = site, fill = site), alpha = 0.25) +
    labs(
      x = "Smolt fork length (mm)",
      y = "Number of smolt",
      color = "Detection site",
      fill = "Detection site"
    ) +
    theme_light() +
    facet_wrap(~get(facet_by), ncol = 4, scales = "free_y") +
    theme(strip.text = element_text(color = "black"))

  # if (!is.null(predators_selected())) {
  #   for (predator in predators_selected()) {
  #     thresholds <- predator_thresholds %>%
  #       filter(species == predator)
  #
  #     line_color <- ifelse(predator == "N. Pikeminnow", "darkgreen", "goldenrod")
  #     dash_color <- ifelse(predator == "N. Pikeminnow", "darkgreen", "goldenrod")
  #
  #     p <- p +
  #       geom_vline(
  #         xintercept = thresholds$median,
  #         color = line_color,
  #         linetype = "solid",
  #         show.legend = TRUE,
  #         aes(linetype = "Median", color = "Median")
  #       ) +
  #       geom_vline(
  #         xintercept = thresholds$min,
  #         linetype = "dashed",
  #         color = dash_color,
  #         show.legend = TRUE,
  #         aes(linetype = "Min/Max", color = "Min/Max")
  #       ) +
  #       geom_vline(
  #         xintercept = thresholds$max,
  #         linetype = "dashed",
  #         color = dash_color,
  #         show.legend = FALSE,
  #         aes(linetype = "Min/Max", color = "Min/Max")
  #       )
  #   }
  # }
  #
  # # Add custom legend for color (histogram) and linetype (vlines)
  # p <- p +
  #   scale_fill_manual(
  #     name = "Detection Site",
  #     values = c("steelblue4", "#b47747"),
  #     title = "Detection Site"
  #   ) +
  #   scale_color_manual(
  #     name = "Predator Species",
  #     values = c("darkgreen", "goldenrod"),
  #     title = "Predator Species"
  #   ) +
  #   scale_linetype_manual(
  #     name = "Threshold",
  #     values = c("solid", "dashed"),
  #     title = "Threshold"
  #   ) +
  #   guides(
  #     fill = guide_legend(order = 1),
  #     color = guide_legend(order = 2),
  #     linetype = guide_legend(order = 3)
  #   )

  return(p)
}


