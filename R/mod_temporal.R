#' temporal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_temporal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
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
      label = "Step 2: For the time period of concern selected, You can select from the following forecast publications months:",
      choices = c(1:12) |>
        rlang::set_names(lubridate::month(c(1:12), label = T, abbr = T)),
      selected = c(11,12,1,2, 3, 4),
    )
    )
    ),
    # class="control-label"
    # <div id="foo" class="shiny-text-output" style="color:green;"></div>
    tags$b(tags$span(class= "control-label",
                     "Step 3: For each selected publication month you can set an activation threshold:")),
    uiOutput(ns("lt_ui")),
  )
}

#' temporal Server Functions
#'
#' @noRd
mod_temporal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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

  })
}

## To be copied in the UI
# mod_temporal_ui("temporal_1")

## To be copied in the server
# mod_temporal_server("temporal_1")
