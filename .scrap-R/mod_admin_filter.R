#' admin_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("analysis_level"),
      label = "Select Analysis Level",
      choices = c(
        Country = "adm0_pcode",
        `Admin 1` = "adm1_pcode",
        `Admin 2` = "adm2_pcode",
        `Admin 3` = "adm3_pcode"
      ),
      selected = "adm0_pcode"
    ),
    uiOutput(outputId = ns("sel_adm0")),
    uiOutput(outputId = ns("sel_adm1")),
    uiOutput(outputId = ns("sel_adm2")),
    uiOutput(outputId = ns("sel_adm3"))
  )
}

#' admin_filter Server Functions
#'
#' @noRd
mod_admin_filter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataset_chosen <- reactive({
      req(input$analysis_level)
      ds_id <- stringr::str_remove(input$analysis_level,"_pcode")
      return(ldf[[ds_id]])
    })
    output$sel_adm0 <- renderUI({
      req(input$analysis_level)
      selectInput("sel_adm0",
                  "Admin 0",
                  choices =admin_choices(df = dataset_chosen(),
                                         values = "adm0_pcode",
                                         labels = "adm0_en"),
                  multiple = T
      )
    })
    output$sel_adm1 <- renderUI({
      req(dataset_chosen(),input$sel_adm0)
      selectInput("sel_adm1",
                  "Admin 1",
                  choices =admin_choices(df = dataset_chosen(),
                                         values = "adm1_pcode",
                                         labels = "adm1_en"),multiple = T
      )
    })
    output$sel_adm2 <- renderUI({
      req(dataset_chosen(),input$sel_adm1)
      selectInput("sel_adm2",
                  "Admin 2",
                  choices =admin_choices(df = dataset_chosen(),
                                         values = "adm2_pcode",
                                         labels = "adm2_en"),multiple = T
      )
    })
    output$sel_adm3 <- renderUI({
      req(dataset_chosen(),input$sel_adm2)
      selectInput("sel_adm3",
                  "Admin 3",
                  choices =admin_choices(df = dataset_chosen(),
                                         values = "adm3_pcode",
                                         labels = "adm3_en"),multiple = T
      )
    })

  })
}

## To be copied in the UI
# mod_admin_filter_ui("admin_filter_1")

## To be copied in the server
# mod_admin_filter_server("admin_filter_1")
