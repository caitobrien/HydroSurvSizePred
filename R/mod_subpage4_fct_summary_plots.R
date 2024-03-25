#' summary_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_summary_plot <- function(data_size, data_pred_threshold, data_pred_risk, data_surv, year, show_legend = TRUE){



#size distribution plot
  #sort year
  data_size <- data_size %>%
    dplyr::mutate( year = factor(year, levels = sort(unique(year))))

size_plot <- data_size %>%
  ggplot2::ggplot(ggplot2::aes(x = length)) +
  ggplot2::geom_histogram(ggplot2::aes(fill = site), binwidth = 5, alpha = .5) +
  ggplot2::geom_vline(data = dplyr::filter(data_pred_threshold,  type == "median"), ggplot2::aes(color = species, xintercept = threshold, linetype = type )) +
  ggplot2::scale_fill_manual(values = c("LWG" = "steelblue4", "BON" = "#b47747"),
                    labels = c("steelblue4"= "LWG", "#b47747"= "BON")) +
  ggplot2::scale_color_manual(values = c("N. Pikeminnow" = "black", "Pacific Hake" = "grey"),
                     labels = c("black"="N. Pikeminnow", "grey" ="Pacific Hake")) +
  ggplot2::scale_linetype_manual(values = "solid",
                        labels = "Median") +
  ggplot2::labs(
    x = "Fork length (mm)",
    y= "Number of smolt",
    fill = "Location",
    color = "Predator",
    linetype = "Predator threshold") +
  ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
         color = ggplot2::guide_legend(override.aes = list(shape = 15), order = 2), # change predator legend to squares
         linetype = ggplot2::guide_legend(order = 3)) +
  ggplot2::theme_light() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())


#predation risk plot
pred_plot <- data_pred_risk%>%
  ggplot2::ggplot(ggplot2::aes(y=pct_susceptible, x=site, fill = predator)) +
  ggplot2::geom_bar(stat= "identity", position = ggplot2::position_dodge()) +
  ggplot2::scale_fill_manual (values = c( "Pacific Hake" = "grey", "N. Pikeminnow" = "black"),
                     labels = c( "grey" ="Pacific Hake", "black"="N. Pikeminnow")) +
  ggplot2::labs(y = "Predation risk (%)",
       x = "Location") +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct_susceptible*100, 1), "%")), vjust = -0.5, position = ggplot2::position_dodge(0.9)) +
  ggplot2::scale_y_continuous(limits = c(0, max(data_pred_risk$pct_susceptible) * 1.1)) + # Adjust y-axis limits
  ggplot2::guides(fill = "none") +
  ggplot2::theme_light() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

#survival plot
surv_plot<- data_surv %>%
  dplyr::filter(reach == "LGR_BOA") %>%
  ggplot2::ggplot(ggplot2::aes(x = as.factor(year), y = median/100))+
  ggplot2::geom_pointrange(ggplot2::aes(color= migration,y=median/100, ymin=min/100, ymax=max/100),
                  shape =21,
                  size = 1,
                  lwd =.25,
                  position = ggplot2::position_jitter())+
  ggplot2::labs( y= "Estimated survival (%)",
        x = "Smolt release year",
        # shape = "Predation risk (%)",
        color = "Passage type",
        linetype = "Median") +
  ggplot2::scale_y_continuous(labels = scales::percent_format())+
  ggplot2::guides(color = ggplot2::guide_legend(order = 1)) +
  ggplot2::theme_light() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())


# Modify the ggplot calls to conditionally hide the legend
  size_plot <- size_plot + ggplot2::theme(legend.position = if (show_legend) 'top' else 'none') + ggplot2::ggtitle(paste0("Year of interest: ", year)) #only add title to leftmost plot to left justify title; move as needed
  pred_plot <- pred_plot + ggplot2::theme(legend.position = if (show_legend) 'top' else 'none')
  surv_plot <- surv_plot + ggplot2::theme(legend.position = if (show_legend) 'top' else 'none')

  # Arrange plots with patchwork and add annotation
  top_legends <- (
    (size_plot + pred_plot + surv_plot ) +
      patchwork::plot_layout(guides = if (show_legend) "collect" else "keep")
  )

  # Display the plot
  top_legends
}
