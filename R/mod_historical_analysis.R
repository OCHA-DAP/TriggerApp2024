#' historical_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxGroupInput(
      inputId = ns("sel_aoi"),
      label = "Select area of interest",
      choices = unique(forecast_data$adm0_es),
      selected = unique(forecast_data$adm0_es)
      ),
    # shiny::tableOutput(outputId = )
    # tableOutput(outputId = ns("df_show"))

  )
}

#' historical_analysis Server Functions
#'
#' @noRd
mod_historical_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df_forecast_aoi <- reactive({
      forecast_data |>
        filter_forecast_spatial(
          zones= input$sel_aoi
        )
    })

    df_forecast_temporal <- reactive({
      df_forecast_aoi() |>
        filter_forecast_temporal() |>
        label_seasons()
    })

    df_forecast_aggregated <-  reactive({
      df_forecast_temporal() |>
        aggregate_forecast()
    })




  }
  )

}


## To be copied in the UI
# mod_historical_analysis_ui("historical_analysis_1")

## To be copied in the server
# mod_historical_analysis_server("historical_analysis_1")
