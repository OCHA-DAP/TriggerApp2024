#' checkSelects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_checkSelects_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        textOutput(ns("window_1_varstrings"))
      ),
      column(
        width = 6,
        textOutput(ns("window_2_varstrings"))
      )
    )
  )
}

#' checkSelects Server Functions
#'
#' @noRd
mod_checkSelects_server <- function(id,l1_inputs,l2_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    window_1_varstrings <- reactive({
      # browser()
      p <- paste_varselects(
        l=list(
          l1_inputs$analysis_level(),
          l1_inputs$admin1(),
          l1_inputs$admin2(),
          l1_inputs$admin3(),
          l1_inputs$valid_mo(),
          l1_inputs$pub_mo(),
          l1_inputs$leadtimes()
        ))
      return(p)
    })

    window_2_varstrings <- reactive({
      p <- paste_varselects(
        l=list(
          l2_inputs$analysis_level(),
          l2_inputs$admin1(),
          l2_inputs$admin2(),
          l2_inputs$admin3(),
          l2_inputs$leadtimes(),
          l2_inputs$valid_mo(),
          l2_inputs$pub_mo(),
          l2_inputs$leadtimes()

        ))
      return(p)
    })
    output$window_1_varstrings <- renderText({window_1_varstrings()})
    output$window_2_varstrings <- renderText({window_2_varstrings()})

    # comment out the above to see if observeEvent will handle
    # browser()
    #
    # observeEvent(l1_inputs$leadtimes(),{
    #   test_txt1 <- paste_varselects(l=list(
    #       l1_inputs$analysis_level(),
    #       l1_inputs$admin1(),
    #       l1_inputs$admin2(),
    #       l1_inputs$admin3(),
    #       l1_inputs$valid_mo(),
    #       l1_inputs$pub_mo(),
    #       l1_inputs$leadtimes()
    #     ))
    #
    #   output$window_1_varstrings <- renderText({test_txt1})
    # }
    # )
    #
    #
    # observeEvent(l2_inputs$leadtimes(),{
    #   test_txt2 <- paste_varselects(
    #     l=list(
    #       l2_inputs$analysis_level(),
    #       l2_inputs$admin1(),
    #       l2_inputs$admin2(),
    #       l2_inputs$admin3(),
    #       l2_inputs$leadtimes(),
    #       l2_inputs$valid_mo(),
    #       l2_inputs$pub_mo(),
    #       l2_inputs$leadtimes()
    #
    #     ))
    #   output$window_2_varstrings <- renderText({test_txt2})
    # })




  })
}

## To be copied in the UI
# mod_checkSelects_ui("checkSelects_1")

## To be copied in the server
# mod_checkSelects_server("checkSelects_1")
