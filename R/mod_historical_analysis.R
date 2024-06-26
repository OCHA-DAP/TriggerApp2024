
# UI ----------------------------------------------------------------------


#' historical_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_historical_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
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
                       choices= rlang::set_names(ldf$adm1$adm1_pcode,ldf$adm1$adm1_en),multiple = T)
      ),
      conditionalPanel(
        ns=ns,
        condition = "input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
        selectizeInput(
          ns("sel_adm2"),
          label = "Admin 2",
          choices= "",
          multiple = T)
      ),
      conditionalPanel(
        ns=ns,
        condition = "input.analysis_level=='adm3_pcode'",
        selectizeInput(
          ns("sel_adm3"),
          label = "Admin 3",
          choices= "",multiple = T)
      )
    ), # end sidebar panel
    mainPanel(
      checkboxGroupInput(inputId = ns("valid_mo1"), # time of interest
                         choices = c(1:12) |>
                           rlang::set_names(lubridate::month(1:12,label=T,abbr=T)),
                         selected = c(5,6,7,8),
                         inline=T,
                         label = "Step 1: Select time period/window of concert"),
      checkboxGroupInput(
        inputId= ns("pub_mo1"),
        label = "2. Here are the available months we can monitor:",
        choices = c(2:5)|>
          rlang::set_names(lubridate::month(c(2:5),label=T,abbr=T)),
        selected = c(2,3,4,5),
        inline = T
      ),
      uiOutput(ns("lt_ui")),
      # radioButtons(inputId = ns("gen"),
      #              label = "Generate",
      #              choices = c(T,F)),
      tableOutput(ns("test_table")),




    )
  )
}


# Server ------------------------------------------------------------------


#' historical_analysis Server Functions
#'
#' @noRd
mod_historical_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Temporal Render UI ####

    ## Render Pub Mo UI ####
    pub_mo_choices <- reactive({
      choices <- find_pub_mos(valid_months = input$valid_mo1)
      rlang::set_names(
        choices,
        lubridate::month(
          choices,
          label=T,abbr=T)
      )
    })



      if(input$analysis_level == "adm0_pcode"){
        data_filtered <-  filter_forecast_temporal(
          df= list_df[["adm0"]],
          publication_month = publication_month,
          valid_month = valid_month
        )
      }
      if(input$analysis_level == "adm1_pcode"){


    }
    filter_data_aoi <- reactive({
      if(input$analysis_level == "adm0_pcode"){
        data_filtered <-  filter_forecast_temporal(
          df= ldf$adm0,
          publication_month = input$pub_mo1,
          valid_month = input$valid_mo1
        )
      }
      if(input$analysis_level == "adm1_pcode"){
        data_filtered <-  ldf$adm1 |>
          filter(adm1_pcode %in% c(input$sel_adm1)) |>
          filter_forecast_temporal(
            df= ldf$adm1,
            publication_month = input$pub_mo1,
            valid_month = input$valid_mo1
          )
      }
      if(input$analysis_level == "adm2_pcode"){
        data_filtered <-  ldf$adm2 |>
          filter(adm2_pcode %in% input$sel_adm2) |>
          filter_forecast_temporal(
            publication_month = input$pub_mo1,
            valid_month = input$valid_mo1
          )
      }
      if(input$analysis_level == "adm3_pcode"){
        data_filtered <-  ldf$adm3 |>
          filter(adm3_pcode %in% input$sel_adm3) |>
          filter_forecast_temporal(
            publication_month = input$pub_mo1,
            valid_month = input$valid_mo1
          )
      }
      return(data_filtered)
    })

    observe({
      print(pub_mo_choices())
      # browser()
      updateCheckboxGroupInput(
        session,
        inputId= "pub_mo1",
        choices = pub_mo_choices(),
        selected = pub_mo_choices(),
        inline = T
      )
    })


    ## Render LT UI ####
    observeEvent(
      list(input$valid_mo1,input$pub_mo1),
      {
        output$lt_ui <-
          renderUI({
            l_lts <- adjustable_leadtimes(publication_months  = as.numeric(input$pub_mo1),
                                          valid_months = as.numeric(input$valid_mo1))
            l_lts |>
              sort() |>
              purrr::map(\(lt){
                sliderInput(
                  inputId = ns(paste0("slider_", lt)),
                  label = paste("Adjust Leadtime ", lt),
                  min = 1, max = 30, value = 4  # Adjust min, max, and value as needed
                )
              })
          })
      }
    )



    update_selects <- reactive({

    })

    # AOI Filtering ####
    observeEvent(
      list(input$valid_mo1,input$pub_mo1),
      {
        if(input$analysis_level=="adm0_pcode" ){
          data_filtered <- filter_forecast_temporal(df = ldf$adm0,
                                                    publication_month = input$pub_mo1,
                                                    valid_month = input$valid_mo1 )
          output$test_table<- renderTable({head(data_filtered)})
        }
        if(input$analysis_level!="adm0_pcode" ){
          adm_filtered_aoi <- ldf$adm2 |>
            dplyr::filter(
              adm1_pcode %in% input$sel_adm1
            )

          adm2_filt_distinct <- adm_filtered_aoi |>
            dplyr::distinct(
              adm2_pcode, adm2_en
            )

          data_filtered <- filter_forecast_temporal(df = adm_filtered_aoi,
                                                    publication_month = input$pub_mo1,
                                                    valid_month = input$valid_mo1 )


          if(!is.null(input$sel_adm1)){
            updateSelectizeInput(session,
                                 inputId = "sel_adm2",
                                 choices = rlang::set_names(adm2_filt_distinct$adm2_pcode,
                                                            adm2_filt_distinct$adm2_en)
            )
          }


          output$test_table<- renderTable({head(data_filtered)})

        }
        if(input$analysis_level=="adm2_pcode" & !is.null(input$sel_adm2)){
          adm_filtered_aoi <- ldf$adm2 |>
            dplyr::filter(
              adm1_pcode %in% input$sel_adm1
            )

          adm2_filt_distinct <- adm_filtered_aoi |>
            dplyr::distinct(
              adm2_pcode, adm2_en
            )

          updateSelectizeInput(session,
                               inputId = "sel_adm2",
                               choices = rlang::set_names(adm2_filt_distinct$adm2_pcode,
                                                          adm2_filt_distinct$adm2_en)
          )
          data_filtered <- filter_forecast_temporal(df = adm_filtered_aoi,
                                                    publication_month = input$pub_mo1,
                                                    valid_month = input$valid_mo1 )
          output$test_table<- renderTable({head(data_filtered)})

        }
        if(input$analysis_level=="adm3_pcode" ){
          adm_filtered_aoi <- ldf$adm2 |>
            dplyr::filter(
              adm1_pcode %in% input$sel_adm1
            )

          adm2_filt_distinct <- adm_filtered_aoi |>
            dplyr::distinct(
              adm2_pcode, adm2_en
            )

          updateSelectizeInput(session,
                               inputId = "sel_adm2",
                               choices = rlang::set_names(adm2_filt_distinct$adm2_pcode,
                                                          adm2_filt_distinct$adm2_en)
          )
          data_filtered <- filter_forecast_temporal(df = adm_filtered_aoi,
                                                    publication_month = input$pub_mo1,
                                                    valid_month = input$valid_mo1 )
          output$test_table<- renderTable({head(data_filtered)})

        }



      })
    observeEvent(
      list(input$sel_adm1,input$pub_mo1), {

        adm_filtered_aoi <- ldf$adm2 |>
          dplyr::filter(
            adm1_pcode %in% input$sel_adm1
          )

        adm2_filt_distinct <- adm_filtered_aoi |>
          dplyr::distinct(
            adm2_pcode, adm2_en
          )

        updateSelectizeInput(session,
                             inputId = "sel_adm2",
                             choices = rlang::set_names(adm2_filt_distinct$adm2_pcode,
                                                        adm2_filt_distinct$adm2_en)
        )
        data_filtered <- filter_forecast_temporal(df = adm_filtered_aoi,
                                                  publication_month = input$pub_mo1,
                                                  valid_month = input$valid_mo1 )
        output$test_table<- renderTable({head(data_filtered)})

      })
    observeEvent(
      list(input$sel_adm2,input$pub_mo1),{
        # browser()

        adm_filtered_aoi <- ldf$adm3 |>
          dplyr::filter(
            adm2_pcode %in% input$sel_adm2
          )

        # this is where the analysis data really gets crunched
        #

        data_filtered <- filter_forecast_temporal(df = adm_filtered_aoi,
                                                  publication_month = input$pub_mo1,
                                                  valid_month = input$valid_mo1 )
        # output$test_table <- renderTable({
        #   head(data_filtered)
        # })

        adm3_filt_distinct <- adm_filtered_aoi |>
          dplyr::distinct(
            adm3_pcode, adm3_en
          )

        updateSelectizeInput(session,
                             inputId = "sel_adm3",
                             choices = rlang::set_names(adm3_filt_distinct$adm3_pcode,
                                                        adm3_filt_distinct$adm3_en)
        )


      })


    # Temporal Filtering ####


    # df_forecast_aoi <- reactive({
    #
    #   forecast_filt <- forecast_data |>
    #     filter_forecast_spatial(
    #       strata_column = "adm0_pcode",
    #       strata_value = "eth"
    #     )
    #  observeEvent(input$sel_adm1,{
    #    forecast_filt <- filter_forecast_spatial(
    #      strata_column = "adm1_pcode",
    #      strata_value = input$sel_adm1
    #    )
    #  })
    #
    # })

    # df_forecast_temporal <- reactive({
    #   df_forecast_aoi() |>
    #     filter_forecast_temporal() |>
    #     label_seasons()
    # })
    #
    # df_forecast_aggregated <-  reactive({
    #   df_forecast_temporal() |>
    #     aggregate_forecast()
    # })
    #



  }
  )

}
# library(TriggerApp2024)
# cka = 1
# ckb = 2
# ckc =NULL
# ckd = NULL
#
# cka %||% ckb
#
# rlang::`%||%`
# rlang::is_null()
# `%||%` <- function(x, y) {
#   if (rlang::is_null(x)) y else x
# }
# ldf$adm3 |>
#   filter(adm1)
# filter_adm <- function(df,
#                        adm0_pcode="a",
#                        adm1_pcode=c("c","b"),
#                        adm2_pcode=NULL,
#                        adm3_pcode=NULL){
#   rlang::raw_deparse_str(adm3_pcode
#                          )
#   rlang::set_names(adm1_pcode,rlang::as_string(quote(adm1_pcode)))
#   args <- list(adm3_pcode,adm2_pcode,adm1_pcode,adm0_pcode)
#   args
#   filter_pcode <- adm3_pcode %||% adm2_pcode %||% adm1_pcode %||% adm0_pcode
#   match(args,filter_pcode)
#   match(filter_pcode,args)
#   ?match.arg(arg = )
#   match.arg(c("gauss", "rect", "ep"),
#             c("gaussian", "epanechnikov", "rectangular", "triangular"),
#             several.ok = TRUE)
# }


## To be copied in the UI
# mod_historical_analysis_ui("historical_analysis_1")

## To be copied in the server
# mod_historical_analysis_server("historical_analysis_1")
