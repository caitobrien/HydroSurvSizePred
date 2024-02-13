#' summary_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_summary_plot <- function(data_size, data_pred_threshold, data_pred_risk, data_surv, year){

#size distribution plot
size_plot <- data_size %>%
  ggplot(aes(x = length)) +
  geom_histogram(aes(fill = site), binwidth = 5) +
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
         color = FALSE)


#predation risk plot
pred_plot <- data_pred_risk%>%
  ggplot(aes(y=pct_susceptible, x=site, fill = predator)) +
  geom_bar(stat= "identity", position = position_dodge()) +
  scale_fill_manual (values = c("N. Pikeminnow" = "darkgreen", "Pacific Hake" = "goldenrod"),
                     labels = c("darkgreen"="N. Pikeminnow", "goldenrod" ="Pacific Hake")) +
  labs(y = "Predation risk (%)",
       x = "Location")



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




# Arrange plots with patchwork and add annotation and legend
top_legends <- (
  (size_plot | pred_plot | surv_plot ) +
    plot_annotation(title = paste0("Year of interest: ", year)) +
    plot_layout(guides = "collect") &
    theme(legend.position='top')
)

# Display the plot
top_legends

}
