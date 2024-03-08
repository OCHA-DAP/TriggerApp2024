#' plot_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        radioButtons(
      inputId = ns("multiple_windows"),
      label = "How many windows?",
      choices = c("1","2"),
      selected = "2",
      inline=T)),
      column(
        3,
    radioButtons(
      inputId = ns("which_charts"),
      label = "Which charts to plot",
      choices = c("None"="none","Combined"="combined","Strata Level"="strata_level"),
      selected = "none",
      inline=T)
      )

  )
  )
}

#' plot_controls Server Functions
#'
#' @noRd
mod_plot_controls_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    list(
      num_windows = reactive({input$multiple_windows}),
      which_charts = reactive({input$which_charts})
    )

  })
}

## To be copied in the UI
# mod_plot_controls_ui("plot_controls_1")

## To be copied in the server
# mod_plot_controls_server("plot_controls_1")
