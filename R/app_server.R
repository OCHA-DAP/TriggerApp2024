# library(dplyr)
# library(TriggerApp2024)
# forecast_data <- load_df_forecast(dataset = "mars")
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_historical_analysis_server("historical_analysis_1")
}
