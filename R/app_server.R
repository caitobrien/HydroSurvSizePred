#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  mod_welcome_page_server("welcome_page_1")

  mod_main_page_server("main_page_1")

  #retrieve reactive values to use in plots and tables as needed
  smoltsize_dataselect_reactives <- mod_subpage1_smoltsize_dataselection_server("subpage1_smoltsize_dataselection_1")

  observe({

    filtered_data <- smoltsize_dataselect_reactives$filtered_data
    predators_selected <- smoltsize_dataselect_reactives$predators_selected
    plot_height <- smoltsize_dataselect_reactives$plot_height


  mod_subpage1_smoltsize_server("subpage1_smoltsize_1", data = filtered_data(), predators_selected = predators_selected(), plot_height = plot_height())
  })

  mod_subpage2_predrisk_server("mod_subpage2_predrisk_1", data = df_pred_summary)

  mod_subpage3_survival_server("subpage3_survival_1", data = df_survival)

  mod_subpage4_summary_server("subpage4_summary_1", data_size = df_fish, data_pred_threshold = pred_long, data_pred_risk = df_pred_summary, data_surv = df_survival)

  mod_background_page_server("background_page_1")


}
