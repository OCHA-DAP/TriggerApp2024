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
    fluidRow(
      column(4,
             tableOutput(outputId = ns("tbl_joint_rate_per_window")),
             ),
      column(
        4,
        tableOutput(outputId = ns("tbl_joint_rate_overall")),
      )
    )
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
        l_w1_inputs$df_window_compare(),
        l_w2_inputs$df_window_compare()
      )

      flagged_by_window_yr <- df_historical_combined |>
        any_flagged_by(grp_vars = c("window","yr_date"),
                       lgl_flag = "lgl_flag")

      joint_activation_rates_per_window <- flagged_by_window_yr |>
        activation_rates_by(grp_vars = "window",lgl_flag = "lgl_flag")

      joint_activation_rates_overall <- flagged_by_window_yr |>
        any_flagged_by(grp_vars = c("yr_date"),
                       lgl_flag = "lgl_flag") |>

        activation_rates_by(grp_vars = NULL,
                            lgl_flag = "lgl_flag")



      list(
        joint_rates_per_window = joint_activation_rates_per_window,
        joint_rates_overall = joint_activation_rates_overall
      )
    })

    output$tbl_joint_rate_per_window <-  gt::render_gt({
      l_combined_window()$joint_rates_per_window |>
        gt::gt() |>
        gt::tab_header("Joint rates by window") |>
        gt::cols_label(
          overall_activation= "Joint Activation",
          overall_rp = "Joint RP"
        ) |>
        gt::fmt_percent(columns = "overall_activation") |>
        gt::fmt_number(columns ="overall_rp",decimals = 1)

      })
    output$tbl_joint_rate_overall <-  gt::render_gt({
      l_combined_window()$joint_rates_overall |>
        gt::gt() |>
        gt::tab_header("Joint rates across windows") |>
        gt::cols_label(
          overall_activation= "Joint Activation",
          overall_rp = "Joint RP"
        ) |>
        gt::fmt_percent(columns = "overall_activation") |>
        gt::fmt_number(columns ="overall_rp",decimals = 1)
      })


  })
}

## To be copied in the UI
# mod_combine_windows_ui("combine_windows_1")

## To be copied in the server
# mod_combine_windows_server("combine_windows_1")
