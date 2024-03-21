#' historical_main_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_main_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
      tableOutput(outputId = ns("tbl_strata_level")),
      ),
      column(
        width = 6,
        uiOutput(outputId = ns("which_plot_ui")),
        plotOutput(outputId = ns("plot_historical_timeseries")),
      )

    )

    # tableOutput(outputId = ns("tbl_strata_combined")),



  )
}

#' historical_main_viz Server Functions
#'
#' @noRd
mod_historical_main_viz_server <- function(id,l_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observe({
    #   if(!("0" %in% names(l_inputs$leadtime()))){
    #     browser()
    #   }
    # })
    # this is how you can VIEW reactives passeed from another module
    ldf_historical <-
      reactive({
        # ldf_historical <-
        run_thresholding(
          df = l_inputs$df_filt(),
          valid_months = l_inputs$valid_mo(),
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
      num_strata <-  length(unique(l_inputs$df_filt()[[l_inputs$analysis_level()]]))
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

    output$which_plot_ui <- renderUI({
      # browser()
      col_nums <- readr::parse_number(names(l_inputs$df_filt()))
      num_chr <- max(col_nums,na.rm=T)
      pcode_col <-  paste0("adm",num_chr,"_pcode")
      en_col <-  paste0("adm",num_chr,"_en")
      groupers <-  c(pcode_col,en_col)
      df_lookup <- l_inputs$df_filt() |>
        dplyr::distinct(!!!rlang::syms(groupers))

      choices_plot<- rlang::set_names(df_lookup[[pcode_col]], df_lookup[[en_col]])
      selectInput(inputId = ns("strata_plot_below"),
                  label = "Plot Timeries",
                  choices =choices_plot
      )
    })


    # would suspect that this being added to reactive above woud improve performance, but not sure
    output$plot_historical_timeseries <-  renderPlot({
      col_nums <- readr::parse_number(names(l_inputs$df_filt()))
      num_chr <- max(col_nums,na.rm=T)
      pcode_col <-  paste0("adm",num_chr,"_pcode")
      # en_col <-  paste0("adm",num_chr,"_en")
      # groupers <-  c(pcode_col,en_col)
      # df_lookup <- l_inputs$df_filt() |>
      #   dplyr::distinct(!!!rlang::syms(groupers))
      # browser()

      month_aggregated_label <- glue::glue_collapse(
        lubridate::month(as.numeric( l_inputs$valid_mo()), label = T),
        sep = "-"
      )
      p_title_main <- glue::glue(
        "Historical {month_aggregated_label}"
      )

      plot_historical(
        df = ldf_historical()$historical_classified |>
          dplyr::filter(
            !!sym(pcode_col) == input$strata_plot_below
            ),
        analysis_level = l_inputs$analysis_level(),
        plot_title = p_title_main
      )
    }
    )

    # it only really makes sense to compare the combined strata across window
    # at least as as an MVP first step.
    # observe({

    yearly_flags_to_compare_with_other_windows <- reactive({

      # so basically if 1 strata --- we can just call this the combined strata
      if(length(unique(ldf_historical()$yearly_flags_lgl[[l_inputs$analysis_level()]]))==1){
        ret <- ldf_historical()$yearly_flags_lgl
      }
      else{
        ret <-  ldf_historical()$yearly_flags_lgl_combined
      }

      return(
        ret |>
          dplyr::select(yr_date,lgl_flag) |>
          dplyr::mutate(
            window = l_inputs$window_name()
          )
      )
      # if >= 1 strata ... let's take the one we combined in run_thresholds
    })


    return(
      list(
        # df_any_activation_per_year =reactive({ldf_historical()$yearly_flags_lgl}),
        df_window_compare =reactive({yearly_flags_to_compare_with_other_windows()}),
        analysis_level =reactive({l_inputs$analysis_level()})
      )
    )
  })
}

## To be copied in the UI
# mod_historical_main_viz_ui("historical_main_viz_1")

## To be copied in the server
# mod_historical_main_viz_server("historical_main_viz_1")
