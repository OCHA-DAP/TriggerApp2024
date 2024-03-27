#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  gghdx::load_source_sans_3()
  # ldf <- load_df_forecast(dataset = "mars_eth")
  ldf <- load_df_forecast(dataset = "combined")
  # ldf <- load_df_forecast(dataset = "mars_eth")
  # df_area_lookup <- arrow::read_parquet(file.path("data","df_eth_admin_area_lookup.parquet"))
  df_area_lookup <- arrow::read_parquet(file.path("data","df_admin_area_lookup.parquet"))
  lgdf <- readr::read_rds(file.path("data","lgdf_combined.rds"))

  # mod_historical_process_simp_server("historical_process_simp_1")
  # browser()
  mod_intro_server("intro_1")
  # mod_admin_filter_server("admin_filter_1")
  w1_adm_inputs <- mod_admin_cascade_server("window_1")
  w1_data_classified <- mod_rp_analysis_individual_server("window_1",l_inputs = w1_adm_inputs)


  w2_adm_inputs <- mod_admin_cascade_server("window_2")
  w2_data_classified <- mod_rp_analysis_individual_server("window_2",l_inputs = w2_adm_inputs)

  mod_combine_windows_server("combine_windows",
                             l_w1_inputs =w1_data_classified,
                             l_w2_inputs = w2_data_classified
  )
  # w1_temporal <-  mod_temporal_server("temporal_1")


  # observe({
  #   browser()
  #   w1_adm_inputs$df_filtered_spatial()
  # })
#
  # w1_var_inputs <- mod_varselect_server("window_1")
  # w2_var_inputs <- mod_varselect_server("window_2")
#
#   # useful module for trouble shooting reactivity, but output is messy
#   # mod_checkSelects_server("checkSelects_1",go
#   #                         l1_inputs=w1_var_inputs,
#   #                         l2_inputs=w2_var_inputs)
#
#   # control_plot_inputs <- mod_plot_controls_server("plot_controls_1")
#
  # w1_data_classified <- mod_historical_main_viz_server("historical_window_1",
  #                                l_inputs = w1_var_inputs)
  #
  # w2_data_classified <- mod_historical_main_viz_server("historical_window_2",
  #                                l_inputs = w2_var_inputs)
  #

}
