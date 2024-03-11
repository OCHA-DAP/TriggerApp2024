#' compile_design_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compile_design_params_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("compiled_params"))
  )

}

#' compile_design_params Server Functions
#'
#' @noRd
mod_compile_design_params_server <- function(id,l_intro_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$compiled_params <-
      renderUI({
        l_intro_inputs$num_windows() |>
          purrr::map(
            \(win_num_tmp){
              renderUI(
                {
                  mod_varselect_ui(
                    id= paste0("window_",win_num_tmp),
                    window_label=paste0("Window ",win_num_tmp)
                  )
                }
              )

              # mod_historical_main_viz_ui(
              #   ns(paste0("historical_window_",win_num_tmp)))
            })

      }
      )


  })
}

## To be copied in the UI
# mod_compile_design_params_ui("compile_design_params_1")

## To be copied in the server
# mod_compile_design_params_server("compile_design_params_1")
