#' combine_windows UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_combine_windows_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(outputId = ns("tbl_joint_rate"))

  )
}

#' combine_windows Server Functions
#'
#' @noRd
mod_combine_windows_server <- function(id,l_w1_inputs, l_w2_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    l_combined_window <- reactive({

      # this is each window classified per year per lead time
      df_historical_combined <- dplyr::bind_rows(

        l_w1_inputs$df_any_activation_per_year() |>
          dplyr::mutate(
            window = "window 1"
          ),
        l_w2_inputs$df_any_activation_per_year()  |>
          dplyr::mutate(
            window = "window 2"
          )
      )
      df_any_window_any_lt <- df_historical_combined |>
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of(
            dplyr::matches("^adm\\d_[pe]")
          )
        ),
        yr_date) |>
        dplyr::summarise(
          lgl_flag = any(lgl_flag), .groups = "drop_last"
        )


      df_joint_activation_rates <- df_any_window_any_lt |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag, na.rm = T),
          overall_rp = 1 / overall_activation
        )

      list(
        combined_window_activation_rates = df_joint_activation_rates
      )
    })

    output$tbl_joint_rate <-  renderTable({l_combined_window()$combined_window_activation_rates})


  })
}

## To be copied in the UI
# mod_combine_windows_ui("combine_windows_1")

## To be copied in the server
# mod_combine_windows_server("combine_windows_1")
