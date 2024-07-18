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
    # h3("Combined activation/return periods across windows"),
    # fluidRow(
    #   column(4,
    #          tableOutput(outputId = ns("tbl_joint_rate_per_window")),
    #          ),
    #   column(
    #     4,
    #     tableOutput(outputId = ns("tbl_joint_rate_overall")),
    #   )
    # )
    fluidRow(
      # column(3,
      #        h3("Combined activation/return periods across windows")
      #        ),
      column(12,
             htmlOutput(outputId = ns("joint_rate_text")),
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

    output$joint_rate_text <-  renderUI({
      joint_rp <- l_combined_window()$joint_rates_overall$overall_rp
      joint_ar <- l_combined_window()$joint_rates_overall$overall_activation

      joint_ar_txt <-  paste0(round(joint_ar,2)*100," %")
      joint_rp_txt <-  round(joint_rp,1)

      HTML(glue::glue(
        '<span style="font-size: 18px;">Across all windows:</span>
      <span style="font-size: 24px;">
      1 in {joint_rp_txt} year return period ({joint_ar_txt})
      </span>'
      )
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
