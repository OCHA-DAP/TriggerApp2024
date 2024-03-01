#' varselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_varselect_ui <- function(id){
  ns <- NS(id)
  tagList(
      inputPanel(
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
          condition = "input.analysis_level =='adm0_pcode'|input.analysis_level=='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
          selectizeInput(ns("sel_adm0"),
                         label = "Admin 0",
                         # selected = "ET02",
                         choices = rlang::set_names(
                           ldf$adm0 |>
                             dplyr::distinct(adm0_pcode, adm0_en) |>
                             dplyr::pull(adm0_pcode),
                           ldf$adm0 |>
                             dplyr::distinct(adm0_pcode, adm0_en) |>
                             dplyr::pull(adm0_en)
                         ),
                         multiple = T
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
          selectizeInput(ns("sel_adm1"),
                         label = "Admin 1",
                         # selected = "ET02",
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
          condition = "input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
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
          condition = "input.analysis_level=='adm3_pcode'",
          selectizeInput(
            ns("sel_adm3"),
            label = "Admin 3",
            choices = "",
            multiple = T
          )
        )
      ),
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
        )
        # might need another col to add up to 12?
      )
      )
}

#' varselect Server Functions
#'
#' @noRd
mod_varselect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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
    # removing observeEvent...
    # observeEvent(
    #   list(input$valid_mo1, input$pub_mo1),
    #   {
        output$lt_ui <-
          # could probably wrap this all: `sliders_ui()` -- not sure any more
          renderUI({

            available_lts <-  available_lts(
              publication_months = as.numeric(input$pub_mo1),
              valid_months = as.numeric(input$valid_mo1)
            )

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

        return(
          list(
            analysis_level = reactive({ input$analysis_level }),
            admin1 = reactive({ input$sel_adm1 }),
            admin2 = reactive({ input$sel_adm2}),
            admin3 = reactive({ input$sel_adm3}),
            valid_mo = reactive({ input$valid_mo1}),
            pub_mo = reactive({ input$pub_mo1}),
            leadtimes =reactive({get_slider_values(input = input,publication_months = input$pub_mo1, valid_months = input$valid_mo1)})
          )
        )
      # }
    # )



    # browser()


        #   reactive({
        #      thresholds_from_sliders(
        #     input = input,
        #     df = data_aggregated,
        #     valid_months = as.numeric(input$valid_mo1),
        #     publication_months = as.numeric(input$pub_mo1)
        #   )
        # })
        # leadtimes = reactive({input$lt_ui})
        # leadtimes = reactive({
        #   get_slider_values(input = input,
        #                     publication_months = input$pub_mo1,
        #                     valid_months  = input$valid_mo1)})
        # leadtimes = reactive(
        #   {
        #     # req(input$lt_ui)
        #   lts = available_lts(
        #   publication_months = as.numeric(input$pub_mo1),
        #   valid_months= as.numeric(input$valid_mo1)
        # )
        #
        # lt_id_tags <- names(lts)
        # browser()
        #
        #
        # purrr::set_names(lt_id_tags,lt_id_tags) |>
        #   purrr::map(\(lt){
        #     slider_val <- input[[paste0("slider_", lt)]] %||% 1000
        #     # isolate(sli)
        #     return(
        #       as.numeric(slider_val)
        #       )
        #   })
        # }
        # )

      # )
    # )


  })
}

## To be copied in the UI
# mod_varselect_ui("varselect_1")

## To be copied in the server
# mod_varselect_server("varselect_1")
