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
    h1("combined activation/return periods across windows"),
    tableOutput(outputId = ns("tbl_joint_rate_per_window")),
    tableOutput(outputId = ns("tbl_joint_rate_overall")),

  )
}

#' combine_windows Server Functions
#'
#' @noRd
mod_combine_windows_server <- function(id,l_w1_inputs, l_w2_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    l_combined_window <- reactive({

      # observe({
      #   num_strata <-
      #   browser()
      # })

      # this is each window classified per year per lead time
      df_historical_combined <- dplyr::bind_rows(
        l_w1_inputs$df_window_compare(),
        l_w2_inputs$df_window_compare()
      )
      # browser()
      df_yearly_any_lt_per_window <-
        df_historical_combined |>
        dplyr::group_by(
          window,
          yr_date
        ) |>
        dplyr::summarise(
          lgl_flag = any(lgl_flag),
          .groups = "drop"
        )

      joint_activation_rates_per_winow <- df_yearly_any_lt_per_window |>
        dplyr::group_by(
          window
        ) |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag, na.rm = T),
          overall_rp = 1 / overall_activation
        )
      joint_activation_rates_overall <- df_yearly_any_lt_per_window |>
        dplyr::group_by(yr_date) |>
        dplyr::summarise(
          lgl_flag = any(lgl_flag),
          .groups="drop"
        ) |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag, na.rm = T),
          overall_rp = 1 / overall_activation
        )


      # df_any_window_any_lt <- df_historical_combined |>
      # dplyr::group_by(
      #   dplyr::across(
      #     dplyr::any_of(
      #       dplyr::matches("^adm\\d_[pe]")
      #     )
      #   ),
      #   yr_date) |>
      #   dplyr::summarise(
      #     lgl_flag = any(lgl_flag), .groups = "drop_last"
      #   )

#
#       df_joint_activation_rates <- df_any_window_any_lt |>
#         dplyr::summarise(
#           overall_activation = mean(lgl_flag, na.rm = T),
#           overall_rp = 1 / overall_activation
#         )

      list(
        joint_rates_per_window = joint_activation_rates_per_winow,
        joint_rates_overall = joint_activation_rates_overall
      )
    })

    output$tbl_joint_rate_per_window <-  gt::render_gt({l_combined_window()$joint_rates_per_window})
    output$tbl_joint_rate_overall <-  gt::render_gt({l_combined_window()$joint_rates_overall})


  })
}

## To be copied in the UI
# mod_combine_windows_ui("combine_windows_1")

## To be copied in the server
# mod_combine_windows_server("combine_windows_1")
