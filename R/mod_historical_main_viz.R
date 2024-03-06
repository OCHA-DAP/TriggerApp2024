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
    ldf_historical <-
      reactive({
        # browser()
      # ldf_historical <-
        run_thresholding(df = l_inputs$df_filt(),valid_months = l_inputs$valid_mo(),leadtimes = l_inputs$leadtimes(),analysis_level = l_inputs$analysis_level())
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
        df = ldf_historical()$historical_classified,
        analysis_level = l_inputs$analysis_level(),
        plot_title = p_title_main
      )
    }
    )
    return(
      list(
        df_any_activation_per_year =reactive({ldf_historical()$yearly_flags_lgl}),
        analysis_level =reactive({l_inputs$analysis_level()})
      )
    )
  })
}

## To be copied in the UI
# mod_historical_main_viz_ui("historical_main_viz_1")

## To be copied in the server
# mod_historical_main_viz_server("historical_main_viz_1")
