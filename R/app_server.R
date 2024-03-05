#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  gghdx::load_source_sans_3()
  ldf <- load_df_forecast(dataset = "mars_eth")
  # mod_historical_analysis_server("historical_analysis_1")
  # mod_historical_process_simp_server("historical_process_simp_1")
  # browser()

  w1_var_inputs <- mod_varselect_server("window_1")
  w2_var_inputs <- mod_varselect_server("window_2")
  mod_checkSelects_server("checkSelects_1",
                          l1_inputs=w1_var_inputs,
                          l2_inputs=w2_var_inputs)

  w1_data_classified <- mod_historical_main_viz_server("historical_window_1",
                                 l_inputs = w1_var_inputs)

  w2_data_classified <- mod_historical_main_viz_server("historical_window_2",
                                 l_inputs = w2_var_inputs)

  mod_combine_windows_server("combine_windows",
                             l_w1_inputs =w1_data_classified,
                             l_w2_inputs = w2_data_classified
                             )
}
