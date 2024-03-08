#' varselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_varselect_ui <- function(id,window_label){
  ns <- NS(id)
  tagList(
    inputPanel(
      textInput( inputId = ns("window_name"),
                        label = "Window Name",
                        value = window_label),
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
                       choices = rlang::set_names(
                         ldf$adm0 |>
                           dplyr::distinct(adm0_pcode, adm0_en) |>
                           dplyr::pull(adm0_pcode),
                         ldf$adm0 |>
                           dplyr::distinct(adm0_pcode, adm0_en) |>
                           dplyr::pull(adm0_en)
                       ),
                       selected= "ET",
                       multiple = T
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
        selectizeInput(
          ns("sel_adm1"),
          label = "Admin 1",
          choices = "",
          selected = "",
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
          selected = "",
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
    uiOutput(ns("lt_ui")),

    # put lt_ui and test_threshold elements next to eachother with fluidRow and column  ####
    # fluidRow(
    #   column(
    #     width = 12,
    #     div(
    #       # class = "label-left",
    #       uiOutput(ns("lt_ui"))
    #     )
    #   )
    #   # might need another col to add up to 12?
    # )
  )
}

#' varselect Server Functions
#'
#' @noRd
mod_varselect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Update Admin Choices ####
    # update available choices for admin 1 based on admin 0 selection
    observeEvent(input$sel_adm0, {
      ui_update_admin(
        session,
        input,
        list_df = ldf,
        admin_level_choices = "adm1"

      )
    })

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
        total_lts <- length(available_lts)

        fluidRow(
            available_lts |>
              purrr::imap(\(mo_tmp,lt_tmp){
                pub_mo_slider_chr <- lubridate::month(as.numeric(mo_tmp), abbr = T, label = T)
                slider_label <- glue::glue("{pub_mo_slider_chr} (LT: {lt_tmp})")
                ns_id <- paste0("slider_", lt_tmp)
                slider_default_iso <- isolate(input[[ns_id]] %||% 4)
                column(
                  width = 2,
                sliderInput(
                  # inputId = ns(paste0("slider_", lt_tmp)),
                  inputId = ns(ns_id),
                  label = slider_label,
                  min = 1,
                  max = 20,
                  value = slider_default_iso,
                  width = "100%" # Adjust min, max, and value as needed
                )
                )
              })
          )})

      # })
    # spatial_filter_keys <- reactive({
    #
    #   get_spatial_filter_keys(adm0_input = input$sel_adm0,
    #                           adm1_input =input$sel_adm1 ,
    #                           adm2_input = input$sel_adm2,
    #                           adm3_input =input$sel_adm3,
    #                           analysis_level=input$analysis_level
    #                           )
    # })

    return(
      list(
        window_name = reactive({input$window_name}),
        analysis_level = reactive({ input$analysis_level }),
        admin0= reactive({input$sel_adm0}),
        admin1 = reactive({ input$sel_adm1 }),
        admin2 = reactive({ input$sel_adm2}),
        admin3 = reactive({ input$sel_adm3}),
        valid_mo = reactive({ input$valid_mo1}),
        pub_mo = reactive({ input$pub_mo1}),
        leadtimes =reactive({get_slider_values(input = input,
                                               publication_months = input$pub_mo1,
                                               valid_months = input$valid_mo1)}),
        # spatial_filter_keys = reactive({spatial_filter_keys()}),
        df_filt= reactive({
          # can remove this step if i just rename analysis_level choices to not include _pcode
          df_id <- stringr::str_remove(input$analysis_level,"_pcode")
          df_sel <- ldf[[df_id]]

          df_sel_adm <- df_sel |>
            # dplyr::filter(
            #   !!rlang::sym(spatial_filter_keys()$name) %in%
            #     spatial_filter_keys()$value
            # ) |>
            dplyr::filter(
              if(!is.null(input$sel_adm0)) adm0_pcode %in% input$sel_adm0 else TRUE,
              if(!is.null(input$sel_adm1)) adm1_pcode %in% input$sel_adm1 else TRUE,
              if(!is.null(input$sel_adm2)) adm2_pcode %in% input$sel_adm2 else TRUE,
              if(!is.null(input$sel_adm3)) adm3_pcode %in% input$sel_adm3 else TRUE
            ) |>
            # separating this filter for trouble shooting. Should be able to combine
            dplyr::filter(
              pub_month %in% input$pub_mo1,
              valid_month %in% input$valid_mo1
            ) # could consider adding the lts here? let's just see if this works
        })
      )
    )
  })
}

## To be copied in the UI
# mod_varselect_ui("varselect_1")

## To be copied in the server
# mod_varselect_server("varselect_1")
