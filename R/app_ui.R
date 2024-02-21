# forecast_data <- load_df_forecast(dataset = "mars")
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    # tagList(
    tags$head(
      includeCSS("styles.css")
    ),
    navbarPage(
      title = HTML('<a style="padding-right:0px;padding-top:0px" class = "navbar-brand" href = "https://centre.humdata.org/anticipatory-action/" target="_blank"><img src = "www/centre_banner_greenbg.png" height = "40" style="position: absolute; top: 0; right: 0;"></a><span class="navbar-text" style="font-size: 16px; color: white"><strong>Seasonal Drought Trigger App</strong></span>'),
      mod_historical_process_simp_ui("historical_process_simp_1"),
      mod_layout_ui("layout_1")
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TriggerApp2024"
    )
  )
}
