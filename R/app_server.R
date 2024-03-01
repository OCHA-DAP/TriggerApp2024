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
  w1_var_inputs <- mod_varselect_server("window_1")
  w2_var_inputs <- mod_varselect_server("window_2")
  mod_checkSelects_server("checkSelects_1",
                          l1_inputs=w1_var_inputs,
                          l2_inputs=w2_var_inputs)
}
