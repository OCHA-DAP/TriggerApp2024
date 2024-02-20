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
  mod_historical_process_simp_server("historical_process_simp_1")
}
