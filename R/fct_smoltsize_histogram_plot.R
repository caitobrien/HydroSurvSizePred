#' smoltsize_histogram
#'
#' @description A function used to produce the smolt size histogram
#' @return The return value is a histogram allows for variable facets (by_year, by_month, by_half_month)
#'
#'
#' @noRd
levels(df$by_month)
fct_smoltsize_histogram_plot <- function(data, facet_by, predators_selected){

  #create base plot
   p <- data %>%
    ggplot(aes(x = length)) +
    geom_histogram(aes( fill = site), alpha = 0.25) +
    labs(
      x = "Smolt fork length (mm)",
      y = "Number of smolt",
      color = "Predator Species",
      fill = "Detection site",
      linetype = "Predator unit"
    ) +
    theme_light() +
    facet_wrap(~get(facet_by), ncol = 4, scales = "free_y") +
    theme(strip.text = element_text(color = "black"))


  # If predators_select has a single value, proceed with the plot
  if (length(predators_selected) == 1) {
    species <- predators_selected
    vline_color <- ifelse(species == "N. Pikeminnow", "darkgreen", "goldenrod")

    p <- p +
      geom_vline(
        xintercept = ifelse(species == "N. Pikeminnow", 166.165, 110),
        color = vline_color,
        linetype = "solid",
        show.legend = TRUE,
        aes(linetype = paste0(species, "Median"), color = paste0(species, "Median"))
      )
  } else if (is.null(predators_selected)) {
    # If predators_selected is NULL, do nothing and continue to the next block
  } else {
    # If more than one value is selected, show both median lines on the plot
    p <- p +
      geom_vline(
        xintercept = 166.165,
        color = "darkgreen",
        linetype = "solid",
        show.legend = TRUE,
        aes(linetype = "N. Pikeminnow Median", color = "N. Pikeminnow Median")
      ) +
      geom_vline(
        xintercept = 110,
        color = "goldenrod",
        linetype = "solid",
        show.legend = TRUE,
        aes(linetype = "Pacific Hake Median", color = "Pacific Hake Median")
      )
  }

  return(p)

}


