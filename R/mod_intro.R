#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9,
             h5("To facilitate anticipatory humanitarian assistance for seasonal droughts, a thorough analysis of historical seasonal rainfall forecasts is indispensable. Decision frameworks often hinge on critical rainfall thresholds, serving as triggers for intervention. This application empowers users to calculate rainfall thresholds tailored to desired activation rates and return periods. Furthermore, it facilitates joint probability analysis, providing valuable insights into the concurrent activation rates when considering multiple time periods and lead times.")
      ),
      column(3,
             selectInput(inputId = "num_windows",
                         label = "How many time periods to analyze?
                             how many times could assistance be provided per year",
                         choices= c("1"=1,"2"=2)
             )

      )),
  )
}

#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    list(
      num_windows = reactive({input$num_windows})
    )

  })
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
