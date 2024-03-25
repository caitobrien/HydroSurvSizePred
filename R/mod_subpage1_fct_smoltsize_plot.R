#'fct_smoltsize_histogram_or_density_plot.R
#'
#' @description A function used to produce the smolt size histogram
#' @param data The data frame to plot.
#' @param facet_by The variables to facet by: year, by_month, by_half_month.
#' @param predators_selected  predators thresholds to plot.
#' @param graph graph choice: histo or density.
#' @param locations_selected site to plot .
#' @return The return value is a histo or density plot of size distribution
#'
#'
#' @noRd

fct_smoltsize_histogram_or_density_plot <- function(data, facet_by, predators_selected, years_selected, graph, locations_selected){

  #Determine the geom function based on the user selected graph type
  geom_function <- switch(graph,
                          "Histogram" = ggplot2::geom_histogram(ggplot2::aes(fill = site, alpha = (site == "LWG")), binwidth = 5,  position = "identity"),
                          "Density" = ggplot2::geom_density(ggplot2::aes(fill = site, alpha = (site == "LWG"))))

  #set levels for month and half-month; arrange by site
  data <- data %>%
    dplyr::arrange(year, by_month, day_month) %>%
    dplyr::mutate( by_month = factor(by_month,
                              levels = c("March", "April", "May", "June"), ordered = TRUE),
            by_half_month = factor( by_half_month,
                                    levels = c("Late March","Early April", "Late April", "Early May", "Late May", "Early June"),
                                    ordered = TRUE),
            site = factor(site, levels = c("BON", "LWG")),
            .before = length,
           year = factor(year, levels = sort(unique(year)))
    ) %>%
    dplyr::arrange(site)  # Sort the data by site only

  ## Create the title and subtitle based on facet_by and years_selected() using helper functin condense_years()
  rct.title <- switch(facet_by,
                  "year" = "Annual size distribution",
                  "by_month" = "Monthly size distribution",
                  "by_half_month" = "Half-monthly size distribution")
  rct.subtitle <- paste("Years:", condense_years(years_selected))



  #create base plot
   size_plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = length)) +
    geom_function +
    ggplot2::labs(
      x = "Smolt fork length (mm)",
      y = "Number of smolt",
      fill = "Detection site",
      alpha = NULL,
      title = rct.title,
      subtitle = rct.subtitle
    ) +
     ggplot2::scale_fill_manual (values = c( "BON" = "#b47747", "LWG" = "steelblue4"),
                        labels = c(  "#b47747" = "BON", "steelblue4" = "LWG"))+
     ggplot2::scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1)) +
     ggplot2::guides(alpha = "none")+
    ggplot2::theme_light() +
    # facet_wrap(~get(facet_by), ncol = 4, scales = "free") +
    ggplot2::theme(strip.text = ggplot2::element_text(color = "black"),
          legend.position = "top",
          panel.grid = ggplot2::element_blank())


   # Facet  base plot depending on facet_by input in function
   if (facet_by %in% c("by_month", "by_half_month")) {
     size_plot <- size_plot +
       ggh4x::facet_grid2(year ~ get(facet_by),
                          axes = "x",
                          remove_labels = "y",
                          scales = "free")
   } else if (facet_by == "year") {
     size_plot <- size_plot +
       ggh4x::facet_wrap2(~get(facet_by),
                          ncol = 4,
                          scales = "free",
                          axes = "x",
                          remove_labels = "y")
   }

   #add geom_vlines for predator thresholds // consider moving to data.R or config.R
   # predator thresholds dataframe used for vlines
   vlines_data <- data.frame(
     xintercept = c(166.165, 76, 255, 110, 70, 200), #set thresholds based on ???
     color = c("black", "black", "black", "grey", "grey", "grey"),
     linetype = c("solid", "dashed", "dashed", "solid", "dashed", "dashed"),
     label = c("N. Pikeminnow", "N. Pikeminnow", "N. Pikeminnow",
               "Pacific Hake", "Pacific Hake", "Pacific Hake")
   )

   # Add vertical lines based on selected predators
   if (!is.null(predators_selected)) {
     size_plot <- size_plot +
       ggplot2::labs( color = "Predator species",
             linetype = "Predator threshold") +
       ggplot2::geom_vline(data = vlines_data[vlines_data$label %in% predators_selected, ],
                  ggplot2::aes(xintercept = xintercept, color = color, linetype = linetype),
                  show.legend = TRUE) +
       ggplot2::scale_color_manual(values = c("black" = "black", "grey" = "grey"),
                          labels = c("N. Pikeminnow", "Pacfic hake"),
                          name = "Predator species") +
       ggplot2::scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                             labels = c( "Min/Max", "Median"),
                             name = "Predator threshold") +
       ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(linetype = 0), order = 1), # remove line from site legend
              color = ggplot2::guide_legend(override.aes = list(shape = 15), order = 2), # change predator legend to squares
              linetype = ggplot2::guide_legend(order = 3))
   } else {
     # If no predators are selected, do nothing
   }


   # Define the grouping variables based on facet_by
   group_vars <- switch(facet_by,
                        "year" = c("year"),
                        "by_month" = c("year", "by_month"),
                        "by_half_month" = c("year", "by_half_month"))

   # use the utils function to create summary data in proper format for adding geom_text()
   summary_data <- create_summary_data(data, group_vars)

   # Add geom_text to the plot to show the total number of smolt per site
   size_plot <- size_plot +
     ggplot2::geom_text(data = summary_data, ggplot2::aes(x = 170, y = Inf, label = label), vjust = 1.5, hjust = 0, size = 3)


  # minor aesthetic adjustments
   size_plot <- size_plot +
     ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgrey"),
           strip.text = ggplot2::element_text(colour = 'black'),
           panel.spacing = ggplot2::unit(2, "lines")) +
     ggplot2::expand_limits(y = c(0, NA))  # Adjust y-axis limits as needed


   # Print summary_data
   print(summary_data)

   print(years_selected)
  return(size_plot)

}

