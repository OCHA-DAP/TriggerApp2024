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
          value = FALSE
        )
      ),#,
      column(
        6,
        conditionalPanel(
          ns= ns,
          condition= "input.group_strata==true",
          numericInput(
            inputId=ns("numb_strata_groups"),
            label = "How many groups?",
            value = 1
          ), # numInput
        ), # cond
      ) # col
    ), # frow
    fluidRow(
      column(
        3,
        conditionalPanel(
          ns= ns,
          condition= "input.group_strata==true",
          textInput(
            inputId=ns("group_name1"),
            label = "Group 1",
            value = "Group 1"
          )
        )
      ),
      column(
        3,
        conditionalPanel(
          ns= ns,
          condition= "input.group_strata==true && input.numb_strata_groups>1",
          textInput(
            inputId=ns("group_name2"),
            label = "Group 2",
            value = "Group 2"
          )
        )
      ),
      column(
        3,
        conditionalPanel(
          ns= ns,
          condition= "input.group_strata==true && input.numb_strata_groups>2",
          textInput(
            inputId=ns("group_name3"),
            label = "Group 3",
            value = "Group 3"
          )
        )
      ),
      column(
        3,
        conditionalPanel(
          ns= ns,
          condition= "input.group_strata==true && input.numb_strata_groups>3",
          textInput(
            inputId=ns("group_name3"),
            label = "Group Names",
            value = "Group 4"
          )
        )
      )
    ),
    # conditionalPanel(
    #   ns= ns,
    #   condition= "input.group_strata==true",
    #   uiOutput(ns("group_names"))
    # ),

    fluidRow(
      column(
        12,
        textOutput(ns("txt_test"))

      )
    ),
    fluidRow(
      column(
        12,
        uiOutput(ns("admins_drag")),
      ) # \end col
    ), # end fluidRow,
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
    ),
    # fluidRow(
    #   column(12,
    #          tableOutput(ns("test_strata_table"))
    #          )
    # ),
    fluidRow(
      column(12,
             textOutput(ns("test_bucket_test"))
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

    ## Dynamic Grouping Buckets ####
    # could def have some better object names for the next 2 funcs
    fill_draggable_pool <- reactive({
      fill_strata_grouping_bucket(
        list_of_dfs = ldf,
        input= input
      )
    })
    observeEvent(list(input$group_strata, input$numb_strata_groups),{
      output$admins_drag <- shiny::renderUI({
        conditionalPanel(
          ns=ns,
          condition= "input.analysis_level!='adm0_pcode' && input.group_strata==true",
          dynamic_bucket_list(
            num_groups = as.character(input$numb_strata_groups),
            reservoir = fill_draggable_pool())
        )
      })
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
    # Group NAMES UI ####

    # browser()
    #   ui_create_group_names <- renderUI({
    #     ui_group_names(
    #     input=input,
    #     number_groups = input$numb_strata_groups
    #     )
    #   #
    #   # output$drag_drop <- ui_drag_drop()
    # })
    # output$group_names <-  ui_create_group_names()
    #
    # get_group_names <-  function(input,number_groups="numb_strata_groups"){
    #   grp_nm_ids <- paste0("Group" ,1:input[[number_groups]])
    #   nm_chr <- grp_nm_ids |>
    #     purrr::map_chr(
    #       \(idtmp){
    #         input[[idtmp]]
    #       }
    #     )
    #   glue::glue_collapse(nm_chr,sep = ",")
    # }
    # #
    # output$txt_test <- renderText({
    #   # browser()
    #   get_group_names(input = input, number_groups = "numb_strata_groups")
    # })


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
    # aggregate_forecast_strata_reactive <- reactive({
    #   browser()
    #   aggregate_to_strata(list_df=ldf,
    #                       analysis_level=input$analysis_level,
    #                       # admin_pcode_values=input$sel_adm1%||% input$sel_adm1 %||% input$sel_adm2 %||% input$sel_adm3,
    #                       number_groups= input$numb_strata_groups,
    #                       input=input
    #   )
    # })
    output$test_bucket_test <- renderText({
      glue::glue_collapse(input$sel_adm1 %||%input$rank_list_1,sep = ",")
    })
    output$test_strata_table <-  renderTable({aggregate_forecast_strata_reactive()})

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
