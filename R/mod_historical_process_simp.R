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
    inputPanel(
      radioButtons(inputId = ns("input_mode"),
                   "Choose an input method:",
                   choices = list("Upload File" = "upload",
                                  "Select Input" = "select"),
                   selected = "select"),
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
      conditionalPanel(
        ns = ns,
        condition = "(input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode') & input.input_mode == 'select'",
        selectizeInput(ns("sel_adm1"),
                       label = "Admin 1",
                       #selected = "ET02",
                       choices = rlang::set_names(
                         ldf$adm1 |>
                           dplyr::distinct(adm1_pcode, adm1_en) |>
                           dplyr::pull(adm1_pcode),
                         ldf$adm1 |>
                           dplyr::distinct(adm1_pcode, adm1_en) |>
                           dplyr::pull(adm1_en)
                       ),
                       multiple = T
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.input_mode == 'upload'",
        fileInput(ns("file_adm1"),
                  "Upload File with PCODES:",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      conditionalPanel(
        ns = ns,
        condition = "(input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode') & input.input_mode == 'select'",
        selectizeInput(
          ns("sel_adm2"),
          label = "Admin 2",
          # selected= ,
          choices = "",
          multiple = T
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.analysis_level=='adm3_pcode' & input.input_mode == 'select'",
        selectizeInput(
          ns("sel_adm3"),
          label = "Admin 3",
          choices = "",
          multiple = T
        )
      )
    ),
    tabsetPanel(
      id = "hist_review",
      tabPanel(
        title = "Trigger Development",
        h4("Trigger Design"),
        # Additional content for Tab 1
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("valid_mo1"), # time of interest
          choices = c(1:12) |>
            rlang::set_names(lubridate::month(1:12, label = T, abbr = T)),
          selected = c(10, 11, 12),
          # inline=T,
          label = "Step 1: Select time period/window of concern"
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("pub_mo1"),
          label = "2. Available months to monitor from:",
          choices = c(1:12) |>
            rlang::set_names(lubridate::month(c(1:12), label = T, abbr = T)),
          selected = c(6, 7, 8, 9),
        ),
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
            gt::gt_output(outputId = ns("test_thresholds"))
          )
        ),
        plotOutput(ns("historical_scatter"), height = "600px"),
        #DT::DTOutput(outputId = ns("test_table")),
        textOutput(ns("svs"))
      ),
      tabPanel(
        title = "Performance Metrics",
        h4("Performance Review"),
        column(2,
               # select years
               selectInput(ns("bad_years_select"), "Select Drought Years:",
                           choices = 1980:2022, selected = c(2003, 2005, 2008, 2021, 2022), multiple = TRUE),
               sliderInput(ns("percent_area"), label = "Percent of Area to trigger:",
                           min = 0, max = 100, value = 50),
               selectInput(ns("since_year"), "Since:",
                           choices = 1980:2022, selected = 1998, multiple = F)
        ),
        column(2,
               DT::DTOutput(ns("metrics"))
        ),
        column(8,
               DT::DTOutput(outputId = ns("performance_years"))
        )
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
    # Update Admin Choices ####
    observeEvent(input$input_mode, {
      if (input$input_mode == "select") {
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
      } else {

        # Observe file upload and set admin choices

      }
    })

    admin_choices <- eventReactive(input$file_adm1, {
      req(input$file_adm1)
      admin_choices <- read_csv(input$file_adm1$datapath)
      #ns(admin_choices)

      adm_file <<- unique(admin_choices[[paste0("admin", 1, "Pcode")]])
      return(admin_choices)
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
                slider_default_iso <- isolate(input[[ns_id]] %||% 20)
                sliderInput(
                  # inputId = ns(paste0("slider_", lt_tmp)),
                  inputId = ns(ns_id),
                  label = slider_label,
                  min = 1,
                  max = 100,
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
        if (input$input_mode == "select") {
          pcode_values <- input[[adm_sel_id]]
        } else {
          # getting admin number
          adm_no <- stringr::str_extract(adm_sel_id, "\\d+")
          pcode_values <- unique(admin_choices()[[paste0("admin", adm_no, "Pcode")]])
        }
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
        if (input$input_mode == "select") {
          pcode_values <- input[[adm_sel_id]]
        } else {
          # getting admin number
          adm_no <- stringr::str_extract(adm_sel_id, "\\d+")
          pcode_values <- unique(admin_choices()[[paste0("admin", adm_no, "Pcode")]])
        }
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

      obj1<<-joint_ar
      obj2<<-df_thresholds

      performance_table <- reactive({
        classifier_reactive()$data |>
          mutate(Date = ymd(as.Date(pub_date, origin = "1899-12-30")) %m+% months(lt),
                 Year = year(Date)) |>
          filter(Year >= input$since_year) |>
          group_by(get(input$analysis_level), Year) |>
          summarise(fore_event = any(lgl_flag)) |>
          group_by(Year) |>
          summarise(`Count of Areas Activated` = sum(fore_event, na.rm=T),
                    `Percent of Areas Activated` = round(`Count of Areas Activated` / n(), 3)) |>
          mutate(`Drought Years` = if_else(Year %in% input$bad_years_select, Year, NA),
                 `Metrics` = case_when((`Percent of Areas Activated`*100) >= input$percent_area & !is.na(`Drought Years`) ~ "TP",
                                       (`Percent of Areas Activated`*100) >= input$percent_area & is.na(`Drought Years`) ~ "FP",
                                       (`Percent of Areas Activated`*100) < input$percent_area & !is.na(`Drought Years`) ~ "FN",
                                       (`Percent of Areas Activated`*100) < input$percent_area & is.na(`Drought Years`) ~ "TN",
                                       .default = NA))
      })
      output$performance_years <- DT::renderDT({
        DT::datatable(performance_table(), rownames = F) |>
          DT::formatStyle("Drought Years", target = 'row',
                          backgroundColor = DT::styleInterval(year(today()), c('tomato','white')))
      })
      metrics_compute <- reactive({
        data.frame(Metrics = c("Hit Rate", "Miss Rate", "False Alarm Rate")) |>
          mutate(Value = c(nrow(filter(performance_table(), Metrics == "TP"))/
                             (nrow(filter(performance_table(), Metrics == "TP"))+nrow(filter(performance_table(), Metrics == "FN"))),
                           nrow(filter(performance_table(), Metrics == "FN"))/
                             (nrow(filter(performance_table(), Metrics == "FN"))+nrow(filter(performance_table(), Metrics == "TP"))),
                           nrow(filter(performance_table(), Metrics == "FP"))/
                             (nrow(filter(performance_table(), Metrics == "FP"))+nrow(filter(performance_table(), Metrics == "TN")))),
                 Value = round(Value, 3))

      })
      output$metrics <- DT::renderDT({
        DT::datatable(metrics_compute(), options = list(dom = 't'), rownames = F)
      })


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
        ) |>
        gt::sub_missing(missing_text = "")

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
    #output$test_data <- renderTable({
    #  head(classifier_reactive()$data)
    #})
    #output$test_table <- DT::renderDT({
    #  DT::datatable(classifier_reactive()$data |> filter(lgl_flag))
    #})
    observe({
      obj3 <<- classifier_reactive()$data
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
