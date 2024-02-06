#' smoltsize_histogram
#'
#' @description A function used to produce the smolt size histogram
#' @return The return value is a histogram allows for variable facets (by_year, by_month, by_half_month)
#'
#'
#' @noRd

fct_smoltsize_histogram_or_density_plot <- function(data, facet_by, predators_selected, graph){

  #Determine the geom function based on the user selected graph type
  geom_function <- switch(graph,
                          "Histogram" = geom_histogram(aes(fill = site), binwidth = 5),
                          "Density" = geom_density(aes(fill = site)))

  #create base plot
   p <- data %>%
    ggplot(aes(x = length)) +
    geom_function +
    labs(
      x = "Smolt fork length (mm)",
      y = "Number of smolt",
      fill = "Detection site"
    ) +
     scale_fill_manual (values = c("LWG" = "steelblue4", "BON" = "#b47747"),
                        labels = c("LWG", "BON"))+
    theme_light() +
    # facet_wrap(~get(facet_by), ncol = 4, scales = "free") +
    theme(strip.text = element_text(color = "black"))


   # Facet  base plot depending on facet_by input in function
   if (facet_by %in% c("by_month", "by_half_month")) {
     p <- p + ggh4x::facet_grid2(year ~ get(facet_by), axes = "x", remove_labels = "y", scales = "free")
   } else if (facet_by == "year") {
     p <- p + ggh4x::facet_wrap2(~get(facet_by), ncol = 4, scales = "free", axes = "x", remove_labels = "y")
   }

   #add geom_vlines for predator thresholds
   # predator thresholds dataframe used for vlines
   vlines_data <- data.frame(
     xintercept = c(166.165, 76, 255, 110, 70, 200), #set thresholds based on ???
     color = c("darkgreen", "darkgreen", "darkgreen", "goldenrod", "goldenrod", "goldenrod"),
     linetype = c("solid", "dashed", "dashed", "solid", "dashed", "dashed"),
     label = c("N. Pikeminnow", "N. Pikeminnow", "N. Pikeminnow",
               "Pacific Hake", "Pacific Hake", "Pacific Hake")
   )

   # Add vertical lines based on selected predators
   if (!is.null(predators_selected)) {
     p <- p +
       labs( color = "Predator species",
             linetype = "Predator threshold") +
       geom_vline(data = vlines_data[vlines_data$label %in% predators_selected, ],
                  aes(xintercept = xintercept, color = color, linetype = linetype),
                  show.legend = TRUE) +
       scale_color_manual(values = c("darkgreen" = "darkgreen", "goldenrod" = "goldenrod"),
                          labels = c("N. Pikeminnow", "Pacfic hake"),
                          name = "Predator species") +
       scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                             labels = c("Median", "Min/Max"),
                             name = "Predator threshold")
   } else {
     # If no predators are selected, do nothing
   }


   # # Calculate summary statistics per location
   # summary_data <- data %>%
   #   group_by(
   #     year,
   #     facet_group = ifelse(get(facet_by) %in% c("by_month", "by_half_month"), get(facet_by), site),
   #     site
   #   ) %>%
   #   summarise(total_smolt = n_distinct(tag_id)) %>%
   #   ungroup
   #
   # # Complete to ensure all combinations are represented and fill missing values with 0
   # summary_data <- summary_data %>%
   #   complete(site, nesting(year, facet_group), fill = list(total_smolt = 0))
   #
   # # Add text annotation in the upper right-hand corner
   # # Calculate the range of the y-axis data
   # y_range <- range(data$length)  # Update 'length' to your y-variable
   #
   # # Calculate the desired y-coordinate for positioning the text annotations
   # desired_y <- y_range[2] + (y_range[2] - y_range[1]) * 0.05  # Adjust the factor as needed
   #
   # # Add text annotation for each site separately
   # p <- p +
   #   geom_text(data = summary_data[summary_data$site == "LWG", ],
   #             aes(x = Inf, y = desired_y, label = paste(site, "n:", total_smolt)),
   #             hjust = 1, vjust = 1, size = 3, color = "black", fontface = "bold") +
   #   geom_text(data = summary_data[summary_data$site == "BON", ],
   #             aes(x = Inf, y = desired_y, label = paste(site, "n:", total_smolt)),
   #             hjust = 1, vjust = -1, size = 3, color = "black", fontface = "bold") +


   p <- p +
     theme(strip.background = element_rect(fill = "lightgrey"),
           strip.text = element_text(colour = 'black'),
           panel.spacing = unit(2, "lines")) +
     expand_limits(y = c(0, NA))  # Adjust y-axis limits as needed


  return(p)

}

