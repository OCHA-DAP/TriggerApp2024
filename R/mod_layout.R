#' layout UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_layout_ui <- function(id){
  ns <- NS(id)

    tabPanel(
      title = "Layouting",
        fluidRow(
          inputPanel(

          column(4, shinydashboard::box(title = "box1")),
          column(4, shinydashboard::box(title = "box2")),
          column(4, shinydashboard::box(title = "box3"))
          )
        )
      )

}

#' layout Server Functions
#'
#' @noRd
mod_layout_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_layout_ui("layout_1")

## To be copied in the server
# mod_layout_server("layout_1")
