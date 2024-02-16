#' smoltsize_histogram
#'
#' @description A function used to produce the smolt size histogram
#' @return The return value is a histogram allows for variable facets (by_year, by_month, by_half_month)
#'
#'
#' @noRd

fct_smoltsize_histogram_or_density_plot <- function(data, facet_by, predators_selected, graph, locations_selected){

  #Determine the geom function based on the user selected graph type
  geom_function <- switch(graph,
                          "Histogram" = geom_histogram(aes(fill = site, alpha = (site == "LWG")), binwidth = 5,  position = "identity"),
                          "Density" = geom_density(aes(fill = site, alpha = (site == "LWG"))))

  #set levels for month and half-month; arrange by site
  data <- data %>%
    arrange(year, by_month, day_month) %>%
    mutate( by_month = factor(by_month,
                              levels = c("March", "April", "May", "June"), ordered = TRUE),
            by_half_month = factor( by_half_month,
                                    levels = c("Late March","Early April", "Late April", "Early May", "Late May", "Early June"),
                                    ordered = TRUE),
            site = factor(site, levels = c("BON", "LWG")),
            .before = length,
           year = factor(year, levels = sort(unique(year)))
    ) %>%
    arrange(site)  # Sort the data by site only

  #create base plot
   size_plot <- data %>%
    ggplot(aes(x = length)) +
    geom_function +
    labs(
      x = "Smolt fork length (mm)",
      y = "Number of smolt",
      fill = "Detection site",
      alpha = NULL,
      title = paste0("Annual size distribution")
    ) +
     scale_fill_manual (values = c( "BON" = "#b47747", "LWG" = "steelblue4"),
                        labels = c(  "#b47747" = "BON", "steelblue4" = "LWG"))+
     scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 1)) +
     guides(alpha = "none")+
    theme_light() +
    # facet_wrap(~get(facet_by), ncol = 4, scales = "free") +
    theme(strip.text = element_text(color = "black"),
          legend.position = "top")


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
                             labels = c( "Min/Max", "Median"),
                             name = "Predator threshold") +
       guides(fill = guide_legend(override.aes = list(linetype = 0), order = 1), # remove line from site legend
              color = guide_legend(override.aes = list(shape = 15), order = 2), # change predator legend to squares
              linetype = guide_legend(order = 3))
   } else {
     # If no predators are selected, do nothing
   }


   # Define the grouping variables based on facet_by
   group_vars <- switch(facet_by,
                        "year" = c("year"),
                        "by_month" = c("year", "by_month"),
                        "by_half_month" = c("year", "by_half_month"))

   # Convert the grouping variables to symbols
   group_vars <- rlang::syms(group_vars)

   # Create a summary data frame that contains the total number of smolt per site per year, month, or half-month
   summary_data <- data %>%
     group_by(!!!group_vars, site) %>%
     summarise(nsite = sum(n(), na.rm = TRUE)) %>%
     ungroup()

   # Fill in the missing combinations of your grouping variables and site with 0
   summary_data <- tidyr::complete(summary_data, !!!group_vars, site, fill = list(nsite = 0))

   # Spread the counts into separate columns for each site
   summary_data <- tidyr::spread(summary_data, key = "site", value = "nsite")

   # Ensure both "BON" and "LWG" columns exist
   summary_data$BON <- ifelse(is.na(summary_data$BON), 0, summary_data$BON)
   summary_data$LWG <- ifelse(is.na(summary_data$LWG), 0, summary_data$LWG)

   # Gather the counts back into a single column with the corresponding site names
   summary_data <- tidyr::gather(summary_data, key = "site", value = "nsite", c("BON", "LWG"))

   # Create the label
   summary_data <- summary_data %>%
     mutate(label = paste(site, "n= ", nsite))

   # Combine the labels for "BON" and "LWG"
   summary_data <- summary_data %>%
     group_by(!!!group_vars) %>%
     summarise(label = paste(label, collapse = "\n")) %>%
     ungroup()

   # Add geom_text to the plot to show the total number of smolt per site
   size_plot <- size_plot +
     geom_text(data = summary_data, aes(x = 170, y = Inf, label = label), vjust = 2, hjust = 0, size = 4)

   # # Create a summary data frame that contains the total number of smolt per site per year, month, or half-month
   # summary_data <- data %>%
   #   group_by(!!!group_vars, site) %>%
   #   summarise(nsite = sum(n(), na.rm = TRUE)) %>%
   #   ungroup()
   #
   # # Spread the counts into separate columns for each site
   # summary_data <- tidyr::spread(summary_data, key = "site", value = "nsite")
   #
   # # Check if the "BON" and "LWG" columns exist in summary_data
   # if (!"BON" %in% names(summary_data)) {
   #   summary_data$BON <- 0
   # }
   # if (!"LWG" %in% names(summary_data)) {
   #   summary_data$LWG <- 0
   # }
   #
   # # Gather the counts back into a single column with the corresponding site names
   # summary_data <- tidyr::gather(summary_data, key = "site", value = "nsite", c("BON", "LWG"))
   #
   # # Create the label
   # summary_data <- summary_data %>%
   #   mutate(label = paste(site, "n= ", nsite))
   #
   # # Combine the labels for "BON" and "LWG"
   # summary_data <- summary_data %>%
   #   group_by(!!!group_vars) %>%
   #   summarise(label = paste(label, collapse = "\n")) %>%
   #   ungroup()
   #
   # # Add geom_text to the plot to show the total number of smolt per site
   # size_plot <- size_plot +
   #   geom_text(data = summary_data, aes(x = 170, y = Inf, label = label), vjust = 2, hjust = 0, size = 4)


  # minor aesthetic adjustments
   size_plot <- size_plot +
     theme(strip.background = element_rect(fill = "lightgrey"),
           strip.text = element_text(colour = 'black'),
           panel.spacing = unit(2, "lines")) +
     expand_limits(y = c(0, NA))  # Adjust y-axis limits as needed


   # Print summary_data
   print(summary_data)

  return(size_plot)

}

