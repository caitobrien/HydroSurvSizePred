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

  #set levels for month and half-month
  data<- data %>%
    arrange(year, by_month, day_month) %>%  # Arrange the dataframe
    mutate( by_month = factor(by_month, levels = c("March", "April", "May", "June"), ordered = TRUE),
      by_half_month = factor( by_half_month, levels = c("Late March","Early April", "Late April", "Early May", "Late May", "Early June"), ordered = TRUE))#need a better way to do this


  #create base plot
   size_plot <- data %>%
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
     size_plot <- size_plot + ggh4x::facet_grid2(year ~ get(facet_by), axes = "x", remove_labels = "y", scales = "free")
   } else if (facet_by == "year") {
     size_plot <- size_plot + ggh4x::facet_wrap2(~get(facet_by), ncol = 4, scales = "free", axes = "x", remove_labels = "y")
   }

   #add geom_vlines for predator thresholds // consider moving to data.R or config.R
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
     size_plot <- size_plot +
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
                             name = "Predator threshold") +
       guides(fill = guide_legend(override.aes = list(linetype = 0), order = 1), # remove line from site legend
              color = guide_legend(override.aes = list(shape = 15), order = 2), # change predator legend to squares
              linetype = guide_legend(order = 3))
   } else {
     # If no predators are selected, do nothing
   }


   size_plot <- size_plot +
     theme(strip.background = element_rect(fill = "lightgrey"),
           strip.text = element_text(colour = 'black'),
           panel.spacing = unit(2, "lines")) +
     expand_limits(y = c(0, NA))  # Adjust y-axis limits as needed




  return(size_plot)

}

