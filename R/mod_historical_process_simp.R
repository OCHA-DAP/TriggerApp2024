#' historical_process_simp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_process_simp_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Historical Analysis",
    fluidRow(
      column(
        3,
        selectInput(
          inputId = ns("analysis_level"),
          label = "Select Analysis Level",width = "100%",
          choices = c(
            Country = "adm0_pcode",
            `Admin 1` = "adm1_pcode",
            `Admin 2` = "adm2_pcode",
            `Admin 3` = "adm3_pcode"
          ),
          selected = "adm0_pcode"
        ) # end selinput
      ), # end column
      column(
        3,
        conditionalPanel(
          ns = ns,
          condition = "input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
          selectizeInput(ns("sel_adm1"),
                         label = "Admin 1",
                         selected = "ET02",
                         choices = rlang::set_names(
                           ldf$adm1 |>
                             dplyr::distinct(adm1_pcode, adm1_en) |>
                             dplyr::pull(adm1_pcode),
                           ldf$adm1 |>
                             dplyr::distinct(adm1_pcode, adm1_en) |>
                             dplyr::pull(adm1_en)
                         ),
                         multiple = T
          ) #\sel
        ) #\ cond
      ), #\col,
      column(
        3,
        conditionalPanel(
          ns = ns,
          condition = "input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
          selectizeInput(
            ns("sel_adm2"),
            label = "Admin 2",
            # selected= ,
            choices = "",
            multiple = T
          ) #end sel input
        )# \end conditional
      ),#end column
      column(
        3,
        conditionalPanel(
          ns = ns,
          condition = "input.analysis_level=='adm3_pcode'",
          selectizeInput(
            ns("sel_adm3"),
            label = "Admin 3",
            choices = "",
            multiple = T
          ) # end sel input
        )#end cond panel
      )# end column
    ), #fluidRow

    fluidRow(
      column(
        6,
        checkboxInput(
          inputId=ns("group_strata"),
          label = "Group selected strata?",
          value = TRUE
        )
      ),#,
      column(
        6,
        numericInput(
          inputId=ns("numb_strata_groups"),
          label = "How many groups?",
          value = 2
        )
      )
      ),
      fluidRow(
        column(
          12,
          uiOutput(ns("admins_drag")),
        ) # \end col
      ),
      # br(),
      # br(),
      # br(),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("valid_mo1"), # time of interest
        choices = c(1:12) |>
          rlang::set_names(lubridate::month(1:12, label = T, abbr = T)),
        selected = c(4,5),
        # inline=T,
        label = "Step 1: Select time period/window of concern"
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("pub_mo1"),
        label = "2. Available months to monitor from:",
        choices = c(1:12) |>
          rlang::set_names(lubridate::month(c(1:12), label = T, abbr = T)),
        selected = c(11,12,1,2, 3, 4),
      ),

      # put lt_ui and test_threshold elements next to eachother with fluidRow and column  ####
      fluidRow(
        column(
          width = 6,
          div(
            class = "label-left",
            uiOutput(ns("lt_ui"))
          )
        ),
        column(
          width = 6,
          # div(style='height:100%; overflow-y:scroll', # can consider this if issues w/ table sizing
          gt::gt_output(outputId = ns("test_thresholds"))
        )
      ),
      fluidRow(
        column(12,
               plotOutput(ns("historical_scatter"))
        )
      )
    )
    }

#' historical_process_simp Server Functions
#'
#' @noRd
mod_historical_process_simp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fill_big_bucket <- reactive({
      # browser()
      input_codes_use <- input$sel_adm3 %||% input$sel_adm2 %||% input$sel_adm1
      input_ids <- c("adm3","adm2","adm1")
      adm_inputs <- list(input$sel_adm3 , input$sel_adm2 , input$sel_adm1)
      idx_not_null <- which(purrr::map_lgl(adm_inputs, ~!is.null(.x)))
      min_idx <- min(idx_not_null)
      input_id_use<- input_ids[[min_idx]]

      pcode_col_chr<- paste0(input_id_use,"_pcode")
      lbl_col_chr<- paste0(input_id_use,"_en")

      df_use <-  ldf[[input_id_use]]
      df_use_lookup <- df_use |>
        dplyr::filter(
          !!rlang::sym(pcode_col_chr) %in% input_codes_use
        ) |>
        dplyr::distinct(
          !!rlang::sym(pcode_col_chr),
          !!rlang::sym(lbl_col_chr)
        )
      purrr::map(df_use_lookup[[lbl_col_chr]],
                 \(lbl){
                   tags$div(htmltools::em(lbl))
                 }) |>
        purrr::set_names(df_use_lookup[[pcode_col_chr]])


    })
    output$admins_drag <- shiny::renderUI({
      conditionalPanel( # move up
        ns=ns,
        condition= "input.analysis_level!='adm0_pcode' && input.group_strata==true",
        sortable::bucket_list(
          header = "Drag the items in any desired bucket",
          group_name = "bucket_list_group",
          orientation = "horizontal",

          sortable::add_rank_list(
            text = "All Strata (Drag from Here)",
            labels = fill_big_bucket(),
            input_id = "rank_list_1"
          ),
          sortable::add_rank_list(
            text = "add to Group 1",
            labels = NULL,
            input_id = "rank_list_2"
          ),
          sortable::add_rank_list(
            text = "or Group 2",
            labels = NULL,
            input_id = "rank_list_3"
          )
        )
      )
    })




    # Update Admin Choices ####
    # update available choices for admin 2 based on admin 1 selection
    observeEvent(input$sel_adm1, {
      ui_update_admin(
        session,
        input,
        list_df = ldf,
        admin_level_choices = "adm2"
      )
    })
    # update available choices for admin 3 based on admin 2 selection
    observeEvent(input$sel_adm2, {
      ui_update_admin(
        session,
        input,
        list_df = ldf,
        admin_level_choices = "adm3"
      )
    })

    # Temporal Render UI ####
    ## Render Pub Mo UI ####
    pub_mo_choices <- reactive({
      # browser()
      choices <- find_pub_mos(valid_months = as.numeric(input$valid_mo1))
      rlang::set_names(
        choices,
        lubridate::month(
          choices,
          label = T,
          abbr = T
        )
      )
    })

    observeEvent(
      input$valid_mo1,
      {
        # browser()
        all_choices <- c(1:12) |>
          rlang::set_names(lubridate::month(1:12, label = T, abbr = T))
        choices_available <- pub_mo_choices()
        disabled_choices <- all_choices[!all_choices %in% choices_available]
        shinyWidgets::updateCheckboxGroupButtons(
          session,
          inputId = "pub_mo1",
          choices = all_choices,
          selected = pub_mo_choices(),
          # disabled = T,
          disabledChoices = disabled_choices
        )
      }
    )


    ## Render LT UI ####
    observeEvent(
      list(input$valid_mo1, input$pub_mo1),
      {
        output$lt_ui <-
          # could probably wrap this all: `sliders_ui()`
          renderUI({
            # browser()
            available_lts <-  available_lts(
              publication_months = as.numeric(input$pub_mo1),
              valid_months = as.numeric(input$valid_mo1)
            )
            # l_lts <- adjustable_leadtimes_robust(
            #   publication_months = as.numeric(input$pub_mo1),
            #   valid_months = as.numeric(input$valid_mo1)
            # )



            # l_lts |>
            # sort() |>
            # rev() |> # reverse order testing for mental model
            available_lts |>
              purrr::imap(\(mo_tmp,lt_tmp){
                pub_mo_slider_chr <- lubridate::month(as.numeric(mo_tmp), abbr = T, label = T)
                slider_label <- glue::glue("{pub_mo_slider_chr} (LT: {lt_tmp})")
                ns_id <- paste0("slider_", lt_tmp)
                slider_default_iso <- isolate(input[[ns_id]] %||% 4)
                sliderInput(
                  # inputId = ns(paste0("slider_", lt_tmp)),
                  inputId = ns(ns_id),
                  label = slider_label,
                  min = 1,
                  max = 30,
                  value = slider_default_iso,
                  width = "100%" # Adjust min, max, and value as needed
                )
              })
          })
      }
    )

    # Process Forecast --------------------------------------------------------

    aggregate_forecast_reactive <- reactive({
      adm_sel_id <- paste0(
        "sel_",
        stringr::str_remove(input$analysis_level, "_pcode")
      )

      # silly shortcut
      if (input$analysis_level == "adm0_pcode") {
        pcode_values <- "ET"
      } else {
        pcode_values <- input[[adm_sel_id]]
      }
      aggregate_forecast(
        list_df = ldf,
        analysis_level = input$analysis_level,
        publication_month = input$pub_mo1,
        valid_month = input$valid_mo1,
        admin_pcode_name = input$analysis_level,
        admin_pcode_values = pcode_values
      )
    })

    # just go pure reactive
    classifier_reactive <- reactive({
      adm_sel_id <- paste0(
        "sel_",
        stringr::str_remove(input$analysis_level, "_pcode")
      )

      # silly shortcut
      if (input$analysis_level == "adm0_pcode") {
        pcode_values <- "ET"
      } else {
        pcode_values <- input[[adm_sel_id]]
      }
      data_aggregated <- aggregate_forecast(
        list_df = ldf,
        analysis_level = input$analysis_level,
        publication_month = input$pub_mo1,
        valid_month = input$valid_mo1,
        admin_pcode_name = input$analysis_level,
        admin_pcode_values = pcode_values
      )


      df_thresholds <- thresholds_from_sliders(
        input = input,
        df = data_aggregated,
        valid_months = as.numeric(input$valid_mo1),
        publication_months = as.numeric(input$pub_mo1)
      )
      # browser()
      strata_cols <- admin_ids(input$analysis_level, label = F)


      data_classified <- classify_historical(
        df = data_aggregated,
        thresh_table = df_thresholds
      )

      joint_ar <- data_classified |>
        dplyr::group_by(!!!rlang::syms(strata_cols), yr_date) |>
        dplyr::summarise(
          lgl_flag = any(lgl_flag), .groups = "drop_last"
        ) |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag, na.rm = T),
          overall_rp = 1 / overall_activation
        )
      df_thresholds <- df_thresholds |>
        dplyr::left_join(joint_ar, by = strata_cols)





      gt_thresholds <- gt::gt(df_thresholds) |>
        gt::cols_hide(matches("_pcode")) |>
        gt::cols_label(
          # overall_activation ="Joint Activation",
          # overall_rp= "Joint RP",
          .list = lookup_rename_gt(analysis_level = input$analysis_level)
        ) |>
        gt::fmt_percent(columns = "overall_activation") |>
        gt::fmt_number(
          columns = c(dplyr::any_of(c("0", "1", "2", "3", "4", "5", "6")), "overall_rp"),
          decimals = 1
        ) |>
        gt::tab_spanner(
          label = "Thresholds from user-defined RPs",
          columns = c(dplyr::any_of(as.character(c(0:6))))
        )


      month_aggregated_label <- glue::glue_collapse(
        lubridate::month(as.numeric(input$valid_mo1), label = T),
        sep = "-"
      )
      p_title_main <- glue::glue(
        "Historical {month_aggregated_label}"
      )
      # browser()


      return(
        list(
          data = data_classified,
          thresholds = gt_thresholds,
          historical_plot = plot_historical(
            df = data_classified,
            analysis_level = input$analysis_level,
            plot_title = p_title_main
          )
        )
      )
    })
    # classifier_reactive() should not be run 2x like this, so an observe might be better
    output$test_thresholds <- gt::render_gt(
      classifier_reactive()$thresholds
    )
    output$test_data <- renderTable({
      head(classifier_reactive()$data)
    })
    output$historical_scatter <- renderPlot({
      # browser()
      classifier_reactive()$historical_plot
    })
  })
}

## To be copied in the UI
# mod_historical_process_simp_ui("historical_process_simp_1")

## To be copied in the server
# mod_historical_process_simp_server("historical_process_simp_1")
