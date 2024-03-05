#' historical_main_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_main_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(outputId = ns("tbl_module_test")),
    plotOutput(outputId = ns("plot_historical_timeseries")),

  )
}

#' historical_main_viz Server Functions
#'
#' @noRd
mod_historical_main_viz_server <- function(id,l_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # this is how you can VIEW reactives passeed from another module
    ldf_historical <- reactive({
      df_summarised <- summarise_forecast_temporal2(df = l_inputs$df_filt(),valid_month_arg = l_inputs$valid_mo())
      df_thresholds <- threshold_values(df= df_summarised,slider_rps = l_inputs$leadtimes())

      # classify at each leadtime
      df_historical_classified <-  classify_historical(
          df = df_summarised,
          thresh_table = df_thresholds
        )

      df_joint_activation_rates <- df_historical_classified |>
        dplyr::group_by(!!!rlang::syms(l_inputs$analysis_level()), yr_date) |>
        dplyr::summarise(
          lgl_flag = any(lgl_flag), .groups = "drop_last"
        ) |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag, na.rm = T),
          overall_rp = 1 / overall_activation
        )
      df_thresholds <- df_thresholds |>
        dplyr::left_join(df_joint_activation_rates, by = l_inputs$analysis_level())

      list(
        df_thresholds= df_thresholds,
        df_historical_classified=df_historical_classified

      )
    })

    output$tbl_module_test <- renderTable({
      ldf_historical()$df_thresholds
      })

    # would suspect that this being added to reactive above woud improve performance, but not sure
    output$plot_historical_timeseries <-  renderPlot({
      month_aggregated_label <- glue::glue_collapse(
        lubridate::month(as.numeric( l_inputs$valid_mo()), label = T),
        sep = "-"
      )
      p_title_main <- glue::glue(
        "Historical {month_aggregated_label}"
      )

      plot_historical(
        df = ldf_historical()$df_historical_classified,
        analysis_level = l_inputs$analysis_level(),
        plot_title = p_title_main
      )

    })

    # browser()








  })
}

## To be copied in the UI
# mod_historical_main_viz_ui("historical_main_viz_1")

## To be copied in the server
# mod_historical_main_viz_server("historical_main_viz_1")
