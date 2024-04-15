#' subpage2_submodule_fct_predrisk_smoltsize_plot
#'
#' @description plot function to compare predation risk and smolt size by transport type
#' @param data_size smolt size distribution data to plot.
#' @param predator_thresholds predator size thresholds
#' @param selected_pass_types selected passage types
#' @param selected_predators selected predators
#'
#' @return The return size distribution with highlighted distribution based on user selection
#'
#' @noRd


fct_predrisk_smoltsize_plot <- function(data_size, predator_thresholds, selected_pass_types, selected_predators){

plot <- ggplot2::ggplot(data_size,  ggplot2::aes(x = length)) +
  ggplot2::geom_histogram(binwidth = 5,
                          fill = "white",
                          color = "black",
                          size = 1) +
  ggplot2::labs(  x = "Smolt fork length (mm)",
                  y = "Number of smolt") +
  ggplot2::theme_classic()

# Conditionally add facet_grid()// change title based on selected passtypes
if(is.null(selected_pass_types)) {
  plot + ggplot2::ggtitle("Smolt size distribution at LWG (all passage types) below predator size thresholds")
} else if (length(selected_pass_types) == 1) {
  plot <- plot +
    ggplot2::facet_grid(~pass_type_T_R) +
    ggplot2::ggtitle("Smolt size distribution at LWG by passage type below predator size thresholds")
} else if(length(selected_pass_types) == 2) {
  plot1 <- plot +
    ggplot2::facet_grid(~pass_type_T_R) +
    ggplot2::ggtitle( "Smolt size distribution at LWG by passage type")
  plot2 <- plot +
    ggplot2::ggtitle( "Smolt size distribution at LWG (all passage types) below predator size thresholds")

  plot <- plot1/plot2
}

if (length(selected_pass_types) == 2) {
  plot1 <- plot
  plot2 <- ggplot2::ggplot(data_size,  ggplot2::aes(x = length)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "white", color = "black", size = 1)
  # ggplot2::labs(title = "ROR & T smolt size distribution at LWG below predator size thresholds") +
}


for (predator in selected_predators) {
  threshold <- predator_thresholds$median[predator_thresholds$species == predator]
  if (!is.null(threshold) && any(data_size$length < threshold)) {
    plot <- plot +
      ggplot2::geom_histogram(data = subset(data_size, length < threshold), binwidth = 5, fill = "black", color = "white", alpha=.3)

  }
}

plot

}
