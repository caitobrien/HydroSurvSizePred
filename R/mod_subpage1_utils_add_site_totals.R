#' add_site_totals
#'
#' @description A utils function to help create summary data and labels for adding geom_text() totals per site per facet select- works with fct_smoltsize and dataselection
#'
#' @noRd
#'
#'
#' @param data The data frame to summarize.
#' @param group_vars The variables to group by.
#'
#' @return A data frame with the summary data and labels in format needed for fct_smoltsize.
create_summary_data <- function(data, group_vars) {
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

  return(summary_data)
}
