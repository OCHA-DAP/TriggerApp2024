
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # temporarily reading parquets -- much faster than holding rda files in
  # data folder as you normally would an R package.

  # historical forecast data merged into single parquet
  ldf <- load_df_forecast_parquets(dataset = "combined")

  # admin area lookup table for area-weighted aggregations
  df_area_lookup <- arrow::read_parquet(
    file.path(
      ".data-scrap",
      "orig_external_data",
      "df_admin_area_lookup.parquet")
    )

  # list of sf class data.frames containing simplified spatial boundaries for
  # use in maps.
  lgdf <- readr::read_rds(
    file.path(
      ".data-scrap",
      "orig_external_data",
      "lgdf_combined.rds")
    )


  # 3 main modules for each window:

  #1. intro section
  mod_intro_server("intro_1")

  #2. admin selection
  w1_adm_inputs <- mod_admin_cascade_server("window_1")

  # 3. month selection
  w1_data_classified <- mod_rp_analysis_individual_server(
    "window_1",
    l_inputs = w1_adm_inputs
  )

  # these are repeated for however many windows
  w2_adm_inputs <- mod_admin_cascade_server("window_2")

  w2_data_classified <- mod_rp_analysis_individual_server(
    "window_2",
    l_inputs = w2_adm_inputs
  )

  # one module that puts it all together
  mod_combine_windows_server(
    "combine_windows",
    l_w1_inputs =w1_data_classified,
    l_w2_inputs = w2_data_classified
  )

}
