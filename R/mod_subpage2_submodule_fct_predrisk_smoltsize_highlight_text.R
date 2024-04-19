#' subpage2_submodule_fct_predrisk_smoltsize_text
#'
#' @description text generated when user selects data inputs
#' @param data_size smolt size distribution data to plot.
#' @param predator_thresholds predator size thresholds
#' @param selected_pass_types selected passage types
#' @param selected_predators selected predators
#' @param selected_year selected year
#'
#' @return The return size distribution with highlighted distribution based on user selection
#'
#' @noRd


fct_predrisk_smoltsize_text <- function(data_size, predator_thresholds, selected_pass_types, selected_predators, selected_year){

  percentages <- c()

  if (length(selected_pass_types) > 1) {
    for (predator in selected_predators) {
      threshold <- predator_thresholds$median[predator_thresholds$species == predator]
      if (!is.null(threshold)) {
        filtered_data_both_pass_types <- data_size %>%
          dplyr::filter(length < threshold)
        if (nrow(filtered_data_both_pass_types) > 0) {
          percentage <- round(nrow(filtered_data_both_pass_types) / nrow(data_size) * 100, 2)
          percentages <- c(percentages, paste0("In-river & Transported : ", predator, " = ", percentage, "%"))
        }
      }
    }
  } else {
    for (pass_type_T_ROR in selected_pass_types) {
      for (predator in selected_predators) {
        threshold <- predator_thresholds$median[predator_thresholds$species == predator]
        if (!is.null(threshold)) {
          filtered_data_pass_type <- data_size %>%
            dplyr::filter(pass_type_T_R == pass_type_T_ROR & length < threshold)
          if (nrow(filtered_data_pass_type) > 0) {
            percentage <- round(nrow(filtered_data_pass_type) / nrow(data_size %>% dplyr::filter(pass_type_T_R == pass_type_T_ROR)) * 100, 2)
            percentages <- c(percentages, paste0( pass_type_T_ROR, " : ",  predator, " = ",  percentage, "%"))
          }
        }
      }
    }
  }

  title <- paste0("For ", selected_year, ", percent of smolt distribution below predator threshold:")

  if(!is.null(selected_predators)){
    HTML(paste(title, paste(percentages, collapse = "<br>"), sep = "<br>"))
  } else{ NULL }

}
