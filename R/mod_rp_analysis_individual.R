#' rp_analysis_individual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rp_analysis_individual_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             tableOutput(outputId = ns("tbl_strata_level")),
      )
    )

  )
}

#' rp_analysis_individual Server Functions
#'
#' @noRd
mod_rp_analysis_individual_server <- function(id,l_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ldf_historical <- reactive({
      req(l_inputs$adm_aggregated())
      # browser()
      run_thresholding2(
        df = l_inputs$adm_aggregated(),
        leadtimes = l_inputs$leadtimes(),
        analysis_level = l_inputs$analysis_level()
      )
    })
    thresh_tbl <- reactive({
      # browser()
      thresh_tbl <- ldf_historical()$thresholds |>
        dplyr::mutate(
          level ="strata"
        )
      num_strata <-  length(unique(l_inputs$adm_aggregated()[[l_inputs$analysis_level()]]))
      if(num_strata>1){
        # browser()
        thresh_combined <- ldf_historical()$thresholds_combined |>
          dplyr::select(-dplyr::starts_with("adm_comb")) |>
          dplyr::mutate(
            level ="combined"
          )
        thresh_tbl<- dplyr::bind_rows(thresh_tbl,thresh_combined)
      }

      return(thresh_tbl)
    })

    output$tbl_strata_level <-  gt::render_gt({
      thresh_tbl() |>
        gt::gt() |>
        gt::cols_label(
          .list = lookup_rename_gt(
            analysis_level = l_inputs$analysis_level()
          )
        ) |>
        gt::cols_hide("level") |>
        gt_style_thresh_table(table_type = "strata") |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_style(
          locations = gt::cells_body(
            columns = dplyr::everything(),
            rows = level=="combined"
          ),
          style = list(
            gt::cell_text(color = 'white'),
            gt::cell_fill(color = '#55b284ff')
          )
        )
    })





  })
}

## To be copied in the UI
# mod_rp_analysis_individual_ui("rp_analysis_individual_1")

## To be copied in the server
# mod_rp_analysis_individual_server("rp_analysis_individual_1")
