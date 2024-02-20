#' historical_process_simp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_process_simp_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(
        "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    .btn-default.active {
      background-color: #1EBFB3;
      color: white;
    }
    .btn checkbtn btn-default {
      background-color: #D2F2F0;
      color: white;
    }
}"
      ))
    ),
shiny::inputPanel(
    # sidebarPanel(
      selectInput(
        inputId = ns("analysis_level"),
        label = "Select Analysis Level",
        choices = c(Country = "adm0_pcode",
                    `Admin 1` = "adm1_pcode",
                    `Admin 2` = "adm2_pcode",
                    `Admin 3`= "adm3_pcode"
        ),
        selected = "adm0_pcode"
      ),
      conditionalPanel(
        ns=ns,
        condition = "input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
        selectizeInput(ns("sel_adm1"),
                       label = "Select Admin 1",
                       selected = "ET02",
                       choices= rlang::set_names(ldf$adm1$adm1_pcode,ldf$adm1$adm1_en),
                       multiple = T)
      ),
      conditionalPanel(
        ns=ns,
        condition = "input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
        selectizeInput(
          ns("sel_adm2"),
          label = "Admin 2",
          # selected= ,
          choices= "",
          multiple = T)
      ),
      conditionalPanel(
        ns=ns,
        condition = "input.analysis_level=='adm3_pcode'",
        selectizeInput(
          ns("sel_adm3"),
          label = "Admin 3",
          choices= "",
          multiple = T)
      )
    ), # end sidebar panel
    # mainPanel(
      shinyWidgets::checkboxGroupButtons(inputId = ns("valid_mo1"), # time of interest
                         choices = c(1:12) |>
                           rlang::set_names(lubridate::month(1:12,label=T,abbr=T)),
                         selected = c(5,6,7,8),
                         # inline=T,
                         label = "Step 1: Select time period/window of concern"),

      shinyWidgets::checkboxGroupButtons(
        inputId= ns("pub_mo1"),
        label = "2. Available months to monitor from:",
        choices = c(1:12)|>
          rlang::set_names(lubridate::month(c(1:12),label=T,abbr=T)),
        selected = c(2,3,4,5),
        # inline = T
      ),

      # put lt_ui and test_threshold elements next to eachother with fluidRow and column  ####
      fluidRow(
        column(width=6,
               div(class = "label-left",
               uiOutput(ns("lt_ui"))
               )
        ),
        column(width=6,
               # div(style='height:100%; overflow-y:scroll',

               gt::gt_output(outputId = ns("test_thresholds"))
        )
      ),


      # div(class = "label-left",
      # uiOutput(ns("lt_ui")),
      # ),
      # # tableOutput(ns("table_summarised")),
      # tableOutput(ns("test_thresholds")),
      # tableOutput(ns("test_data")),
      plotOutput(ns("historical_scatter")),
      textOutput(ns("svs"))
    # )
  )
}

#' historical_process_simp Server Functions
#'
#' @noRd
mod_historical_process_simp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update Admin Choices ####
    # update available choices for admin 2 based on admin 1 selection
    observeEvent(input$sel_adm1, {
      updated_choices_adm2 <- update_admin_choices(
        list_df= ldf,
        admin_level_choices = "adm2",
        parent_selection = input$sel_adm1
      )
      updateSelectizeInput(session,
                           inputId = "sel_adm2",
                           choices = updated_choices_adm2
      )
    })
    # update available choices for admin 3 based on admin 2 selection
    observeEvent(input$sel_adm2,{
      updated_choices_adm3 <- update_admin_choices(
        list_df= ldf,
        admin_level_choices = "adm3",
        parent_selection = input$sel_adm2
      )
      updateSelectizeInput(session,
                           inputId = "sel_adm3",
                           choices = updated_choices_adm3
      )
    })

    # Temporal Render UI ####
    ## Render Pub Mo UI ####
    pub_mo_choices <- reactive({
      choices <- find_pub_mos(x = input$valid_mo1)
      rlang::set_names(
        choices,
        lubridate::month(
          choices,
          label=T,abbr=T)
      )
    })

    observeEvent(
      input$valid_mo1,{
        # browser()
        all_choices <- c(1:12) |>
          rlang::set_names(lubridate::month(1:12,label=T,abbr=T))
        choices_available <- pub_mo_choices()
        disabled_choices <-  all_choices[!all_choices %in% choices_available]
        shinyWidgets::updateCheckboxGroupButtons(
          session,
          inputId= "pub_mo1",
          choices =all_choices,
          selected = pub_mo_choices(),
          # disabled = T,
          disabledChoices = disabled_choices
        )


        # updateCheckboxGroupInput(
        #   session,
        #   inputId= "pub_mo1",
        #   choices =all_choices,
        #   selected = pub_mo_choices(),
        #   inline = T,
        #   choicesOpt= list(
        #     disabled =disabled_choices ,
        #     style = ifelse(disabled_choices,
        #                    yes = "color: rgba(119, 119, 119, 0.5);",
        #                    no = "")
        #   )
        #
        # )
        print(pub_mo_choices())
      }

    )


    ## Render LT UI ####
    observeEvent(
      list(input$valid_mo1,input$pub_mo1),
      {
        output$lt_ui <-
          # could probably wrap this all: `sliders_ui()`
          renderUI({
            l_lts <- adjustable_leadtimes(
              publication_months  = as.numeric(input$pub_mo1),
              valid_months = as.numeric(input$valid_mo1)
            )
            l_lts |>
              sort() |>
              purrr::map(\(lt){
                ns_id <- paste0("slider_", lt)
                slider_default_iso <- isolate(input[[ns_id]] %||% 4)
                sliderInput(
                  inputId = ns(paste0("slider_", lt)),
                  label = paste("LT: ", lt),
                  min = 1, max = 30, value = slider_default_iso,width = "100%"  # Adjust min, max, and value as needed
                )
              })
          })
    }
    )

    # Process Forecast --------------------------------------------------------

    aggregate_forecast_reactive <- reactive({
      adm_sel_id <- paste0("sel_",
                           stringr::str_remove(input$analysis_level,"_pcode")
      )

      # silly shortcut
      if(input$analysis_level=="adm0_pcode"){
        pcode_values <-  "ET"
      }else{
        pcode_values <- input[[adm_sel_id]]
      }
      aggregate_forecast(list_df = ldf,
                         analysis_level = input$analysis_level,
                         publication_month = input$pub_mo1,
                         valid_month = input$valid_mo1,
                         admin_pcode_name =input$analysis_level ,
                         admin_pcode_values =pcode_values
      )
    }
    )

    # just go pure reactive
    classifier_reactive <- reactive({
      adm_sel_id <- paste0("sel_",
                           stringr::str_remove(input$analysis_level,"_pcode")
      )

      # silly shortcut
      if(input$analysis_level=="adm0_pcode"){
        pcode_values <-  "ET"
      }else{
        pcode_values <- input[[adm_sel_id]]
      }
      data_aggregated <- aggregate_forecast(list_df = ldf,
                                            analysis_level = input$analysis_level,
                                            publication_month = input$pub_mo1,
                                            valid_month = input$valid_mo1,
                                            admin_pcode_name =input$analysis_level ,
                                            admin_pcode_values =pcode_values
      )


      df_thresholds <- thresholds_from_sliders(
        input = input,
        df = data_aggregated,
        valid_months = input$valid_mo1,
        publication_months = input$pub_mo1
      )
      # browser()
      strata_cols <- admin_ids(input$analysis_level,label = F)


      data_classified <- classify_historical(
        df = data_aggregated,
        thresh_table = df_thresholds
      )

      joint_ar <- data_classified |>
        dplyr::group_by(!!!rlang::syms(strata_cols), yr_date) |>
        dplyr::summarise(
          lgl_flag= any(lgl_flag),.groups="drop_last"
        ) |>
        dplyr::summarise(
          overall_activation = mean(lgl_flag,na.rm = T),
          overall_rp= 1/overall_activation
        )
      df_thresholds <- df_thresholds |>
        dplyr::left_join(joint_ar,by = strata_cols)





      gt_thresholds <- gt::gt(df_thresholds) |>
        gt::cols_hide(matches("_pcode")) |>
        gt::cols_label(
          # overall_activation ="Joint Activation",
          # overall_rp= "Joint RP",
          .list = lookup_rename_gt(analysis_level=input$analysis_level)

          ) |>
        gt::fmt_percent(columns = "overall_activation") |>
        gt::fmt_number(
          columns= c(dplyr::any_of(c("0","1","2","3","4","5","6")),"overall_rp"),
          decimals=1
          ) |>
      gt::tab_spanner(
        label = "Thresholds from user-defined RPs",
        columns = c(dplyr::any_of(as.character(c(0:6))))
      )


      month_aggregated_label <- glue::glue_collapse(
        lubridate::month(as.numeric(input$valid_mo1),label = T),sep = "-"
      )
      p_title_main <- glue::glue(
        "Historical {month_aggregated_label}"
      )
      # browser()


      return(
        list(data= data_classified,
             thresholds = gt_thresholds,
             historical_plot = plot_historical(
               df = data_classified,
               analysis_level = input$analysis_level,
               plot_title = p_title_main
             )


        )
      )
    }
    )
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
