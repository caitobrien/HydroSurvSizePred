#' summary_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_summary_plot <- function(data_size, data_pred_threshold, data_pred_risk, data_surv, year, show_legend = TRUE){

#size distribution plot
size_plot <- data_size %>%
  ggplot(aes(x = length)) +
  geom_histogram(aes(fill = site), binwidth = 5, alpha = .5) +
  geom_vline(data = filter(data_pred_threshold,  type == "median"), aes(color = species, xintercept = threshold, linetype = type )) +
  scale_fill_manual(values = c("LWG" = "steelblue4", "BON" = "#b47747"),
                    labels = c("steelblue4"= "LWG", "#b47747"= "BON")) +
  scale_color_manual(values = c("N. Pikeminnow" = "darkgreen", "Pacific Hake" = "goldenrod"),
                     labels = c("darkgreen"="N. Pikeminnow", "goldenrod" ="Pacific Hake")) +
  labs(
    x = "Fork length (mm)",
    y= "Number of smolt",
    fill = "Location",
    color = "Predator median",
    linetype = NULL) +
  guides(linetype = FALSE,
         color = FALSE) +
  theme_light()

#size_plot<-plotly::ggplotly(size_plot)


#predation risk plot
pred_plot <- data_pred_risk%>%
  ggplot(aes(y=pct_susceptible, x=site, fill = predator)) +
  geom_bar(stat= "identity", position = position_dodge()) +
  scale_fill_manual (values = c( "Pacific Hake" = "goldenrod", "N. Pikeminnow" = "darkgreen"),
                     labels = c( "goldenrod" ="Pacific Hake", "darkgreen"="N. Pikeminnow")) +
  labs(y = "Predation risk (%)",
       x = "Location") +
  geom_text(aes(label = paste0(round(pct_susceptible*100, 1), "%")), vjust = -0.5, position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0, max(data_pred_risk$pct_susceptible) * 1.1)) + # Adjust y-axis limits
  theme_light()

#pred_plot<-plotly::ggplotly(pred_plot)



#survival plot

surv_plot<- data_surv %>%
  filter(reach == "LGR_BOA") %>%
  ggplot(aes(x = as.factor(year), y = median/100))+
  geom_pointrange(aes(color= migration,y=median/100, ymin=min/100, ymax=max/100),
                  shape =21,
                  size = 1,
                  lwd =.25,
                  position = position_jitter())+
  labs( y= "Estimated survival (%)",
        x = "Smolt release year",
        # shape = "Predation risk (%)",
        color = "Passage type") +
  scale_y_continuous(labels = scales::percent_format())+
  theme_light()

#surv_plot<-plotly::ggplotly(surv_plot)




# Modify the ggplot calls to conditionally hide the legend
  size_plot <- size_plot + theme(legend.position = if (show_legend) 'top' else 'none') + ggtitle(paste0("Year of interest: ", year))
  pred_plot <- pred_plot + theme(legend.position = if (show_legend) 'top' else 'none')
  surv_plot <- surv_plot + theme(legend.position = if (show_legend) 'top' else 'none')

  # Arrange plots with patchwork and add annotation
  top_legends <- (
    (size_plot | pred_plot | surv_plot ) +
      plot_layout(guides = if (show_legend) "collect" else "keep")
  )

  # Display the plot
  top_legends
}
