#' admin_cascade UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_cascade_ui <- function(id,
                                 init_valid_months=c(5,6,7,8),
                                 init_pub_months= c(3,4,5),
                                 window_label
                                 ){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2,
        textInput( inputId = ns("window_name"),
                   label = "Window Name",
                   value = window_label),
      )),
    fluidRow(
      column(
        3,
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
    )
    ),
    column(
      2,
    conditionalPanel(
      ns = ns,
      condition = "input.analysis_level =='adm0_pcode'|input.analysis_level=='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
      selectizeInput(ns("sel_adm0"),
                     label = "Admin 0",
                     choices = NULL,
                     multiple = T
      )
      )
    ),
    column(
      2,
    conditionalPanel(
      ns = ns,
      condition = "input.analysis_level =='adm1_pcode'|input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
      selectizeInput(
        ns("sel_adm1"),
        label = "Admin 1",
        choices = NULL,
        # selected = "",
        multiple = T
      )

      )),
    column(
      2,
    conditionalPanel(
      ns = ns,
      condition = "input.analysis_level=='adm2_pcode'|input.analysis_level=='adm3_pcode'",
      selectizeInput(
        ns("sel_adm2"),
        label = "Admin 2",
        choices = NULL,
        # selected = "",
        multiple = T
      )
    )),
    column(
      2,
    conditionalPanel(
      ns = ns,
      condition = "input.analysis_level=='adm3_pcode'",
      selectizeInput(
        ns("sel_adm3"),
        label = "Admin 3",
        choices = NULL,
        multiple = T
      )
    )
    )
    ),

# mod temporal ui ---------------------------------------------------------


    fluidRow(
      column(
        6,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("valid_mo1"), # time of interest
          choices = c(1:12) |>
            rlang::set_names(lubridate::month(1:12, label = T, abbr = T)),
          # selected = c(5,6,7,8),
          selected = init_valid_months,
          # inline=T,
          label = "Step 1: Select time period/window of concern"
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("pub_mo1"),
          label = "Step 2: For the time period of concern selected, You can select from the following forecast publications months:",
          choices = c(1:12) |>
            rlang::set_names(lubridate::month(c(1:12), label = T, abbr = T)),
          selected = init_pub_months
        )
      ),
      column(
        4,
        leaflet::leafletOutput(ns("map_choro"),width = "100%",height = 200),
      )
    ),
    # class="control-label"
    # <div id="foo" class="shiny-text-output" style="color:green;"></div>
    tags$b(tags$span(class= "control-label",
                     "Step 3: For each selected publication month you can set an activation threshold:")),
    uiOutput(ns("lt_ui"))
  )
}

#' admin_cascade Server Functions
#' https://mastering-shiny.org/action-dynamic.html
#' @noRd
mod_admin_cascade_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # thes issue is that its choosing this always as the dataset...
    # so would need an action button to see the correct
    # rendered table ... for example if you select admin 3
    # and then only filter to admin 2 , you will still just get the admin 3 table
    # rendered and filtered to the admin 2... its not going to show the
    # correct table till you get to admin 3... but htats not even really working correctl yet either
    dataset_chosen <- reactive({
      req(input$analysis_level)
      ds_id <- stringr::str_remove(input$analysis_level,"_pcode")
      return(ldf[[ds_id]])
      })

    output$map_choro <- leaflet::renderLeaflet({
      req(input$sel_adm0)
      # browser()

      gdf_adm0 <- lgdf[["adm0_pcode"]] |>
        dplyr::filter(
          adm0_pcode %in% input$sel_adm0
        )
      df_adm_bbox <- unname(sf::st_bbox(gdf_adm0))

      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addPolygons(data=gdf_adm0,
                             fillColor = "white",
                             fillOpacity = 0.7,
                             color = unname(map_line_colors["level_4"]),weight = 1
                             ) |>
      leaflet::fitBounds(
        lng1 =df_adm_bbox[1],lat1 = df_adm_bbox[2],lng2 = df_adm_bbox[3],lat2 = df_adm_bbox[4]
      )
    })

    # okay get the dataset and first provide the country options
    observeEvent(
      list(
      dataset_chosen(),
      req(input$analysis_level %in% c("adm0_pcode","adm1_pcode","adm2_pcode","adm3_pcode"))
      )
      ,{
      df_choices <- dplyr::distinct(dataset_chosen(),adm0_pcode,adm0_en)
      nv_choices <- rlang::set_names(df_choices$adm0_pcode,df_choices$adm0_en)
      # freezeReactiveValue(input, "sel_adm0")
      updateSelectizeInput(inputId = "sel_adm0",
                           choices = nv_choices)
    })
    # filter admin 0
    filt_adm0 <- reactive({
      req(input$sel_adm0)
      # browser()
      print(dataset_chosen())
      dplyr::filter(dataset_chosen(),adm0_pcode %in% input$sel_adm0)
    })

    # populate admin 1 choices
    observeEvent(
      list(
      input$sel_adm0,
      # dataset_chosen(),
      # filt_adm0(),
      req(input$analysis_level %in% c("adm1_pcode","adm2_pcode","adm3_pcode"))
      )
    ,{
      df_choices <- dplyr::distinct(filt_adm0(),adm1_pcode,adm1_en)
      nv_choices <- rlang::set_names(df_choices$adm1_pcode,df_choices$adm1_en)
      freezeReactiveValue(input, ns("sel_adm1"))
      updateSelectizeInput(inputId = "sel_adm1",
                           choices = nv_choices)
    })

    # filter to selected admin 1
    filt_adm1 <-  reactive({
      req(input$sel_adm1)
      dplyr::filter(filt_adm0(),adm1_pcode %in% input$sel_adm1)
    })

    # populate admin 2 choices
    observeEvent(
      list(input$sel_adm1,
           req(input$analysis_level %in% c("adm2_pcode","adm3_pcode"))
           ),{
      df_choices <- dplyr::distinct(filt_adm1(),adm2_pcode,adm2_en)
      nv_choices <- rlang::set_names(df_choices$adm2_pcode,df_choices$adm2_en)
      # freezeReactiveValue(input, "sel_adm2")
      updateSelectizeInput(inputId = "sel_adm2", choices = nv_choices)
    })


    # filter to selected admin 2
    filt_adm2 <-  reactive({
      req(input$sel_adm2)
      dplyr::filter(filt_adm1(),adm2_pcode %in% input$sel_adm2)
    })

    # populate admin 3 choices
    observeEvent(
      list(
        input$sel_adm2,
        req(input$analysis_level %in% c("adm3_pcode"))
        ),
        {
      df_choices <- dplyr::distinct(filt_adm2(),adm3_pcode,adm3_en)
      nv_choices <- rlang::set_names(df_choices$adm3_pcode,df_choices$adm3_en)

      # freezeReactiveValue(input, "sel_adm3")
      updateSelectizeInput(inputId = "sel_adm3", choices = nv_choices)
    })

    # filter to selected admin 3
    filt_adm3 <-  reactive({
      req(input$sel_adm3)
      dplyr::filter(filt_adm2(),adm3_pcode %in% input$sel_adm3)
    })


    map_fill_colors <- c(
      top_layer = "#F2645A", # tomato-hdx
      middle_layer = "#CCCCCC" ,# grey-medium
      bottom_layer = "#EEEEEE"
      )
    map_line_colors <- c(
      level_1 = "#888888", # tomato-hdx
      level_2 = "white" ,# grey-medium
      level_3 = "white",
      level_4= "black"
      )


    #   observe({
    #     if(input$analysis_level=="adm3_pcode"){
    #    browser()
    #     adm0() %||% adm1() %||% adm2() %||% adm3()
    #     }
    # })
    adm_filt <-reactive({
      switch(
        input$analysis_level,
        "adm0_pcode"= filt_adm0(),
        "adm1_pcode"= filt_adm1(),
        "adm2_pcode"= filt_adm2(),
        "adm3_pcode"= filt_adm3()
      )
    })
    adm_aggregated <- reactive({
      req(adm_filt())
      # browser()
      summarise_forecast_temporal_new(df = adm_filt(),
                                     publication_month = as.numeric(input$pub_mo1),
                                     valid_month = as.numeric(input$valid_mo1)
                                     )
    }
    )
    observeEvent(
      list(input$sel_adm1,
           req(input$analysis_level%in% c("adm1_pcode","adm2_pcode","adm3_pcode"))
      ),{
        gdf_adm <- lgdf[["adm1_pcode"]]
        leaflet::leafletProxy(mapId = "map_choro") |>
          # leaflet::clearShapes() |>
          leaflet::addPolygons(data=lgdf[["adm1_pcode"]],
                               fillColor ="white",
                               color = "darkgrey",
                               fillOpacity = 0.5,
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(gdf_adm,adm1_pcode %in% c(input$sel_adm1)),
                               fillColor =unname(map_fill_colors["top_layer"]),color=unname(map_line_colors["level_3"]),
                               fillOpacity = 1,
                               popup = ~adm1_en,
                               label = ~as.character(adm1_en)
                               ) |>
          leaflet::addPolygons(data=lgdf[["adm0_pcode"]] |>
                                 dplyr::filter(adm0_pcode %in% input$sel_adm0),
                               fillColor = NULL,
                               fillOpacity = 0,
                               color = unname(map_line_colors["level_4"]),weight = 1.5
          )
    })
    # Map out admin 2s
    observeEvent(
      list(input$sel_adm1,
           input$sel_adm2,
           req(input$analysis_level%in% c("adm2_pcode","adm3_pcode"))
      )
           ,{
        leaflet::leafletProxy(mapId = "map_choro") |>
          # leaflet::clearShapes() |>
          leaflet::addPolygons(data=lgdf[["adm1_pcode"]],
                               fillColor =NULL,fillOpacity = 0,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm1_pcode"]],adm1_pcode %in% input$sel_adm1),
                               fillColor =unname(map_fill_colors["bottom_layer"]),
                               fillOpacity = 1,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm2_pcode"]],adm1_pcode %in% input$sel_adm1),
                               fillColor =NULL,fillOpacity = 0,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm2_pcode"]],adm2_pcode %in% c(input$sel_adm2)),
                               fillColor =unname(map_fill_colors["top_layer"]),color=unname(map_line_colors["level_2"]), fillOpacity = 1
                               )
    })
    # Map out admin 3s
    observeEvent(
      list(input$sel_adm1,
           input$sel_adm2,
           input$sel_adm3,
           req(input$analysis_level%in% c("adm3_pcode"))
      )
           ,{

        leaflet::leafletProxy(mapId = "map_choro") |>
          # leaflet::clearShapes() |>
          leaflet::addPolygons(data=lgdf[["adm1_pcode"]],
                               fillColor =NULL,fillOpacity = 0,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm1_pcode"]],adm1_pcode %in% input$sel_adm1),
                               fillColor =unname(map_fill_colors["bottom_layer"]),
                               fillOpacity = 1,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm2_pcode"]],adm1_pcode %in% c(input$sel_adm1)),
                               fillColor =NULL,
                               fillOpacity = 0,
                               color = "darkgrey",
                               weight = 1) |>
          leaflet::addPolygons(data=dplyr::filter(lgdf[["adm2_pcode"]],adm2_pcode %in% c(input$sel_adm2)),
                               fillColor =unname(map_fill_colors["middle_layer"]),color=unname(map_line_colors["level_2"]), fillOpacity = 1
                               ) |>
               leaflet::addPolygons(data=dplyr::filter(lgdf[["adm3_pcode"]],adm2_pcode %in% c(input$sel_adm2)),
                                    fillColor =NULL,fillOpacity = 0,
                                    color = "darkgrey",
                                    weight = 1) |>
               leaflet::addPolygons(data=dplyr::filter(lgdf[["adm3_pcode"]],adm3_pcode %in% c(input$sel_adm3)),
                                    fillColor =unname(map_fill_colors["top_layer"]),color=unname(map_line_colors["level_2"]), fillOpacity = 1
               )
    })


# mod_temporal ------------------------------------------------------------


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

    return(
      list(
        window_name = reactive({input$window_name}),
        analysis_level = reactive({ input$analysis_level }),
        adm_aggregated = reactive({adm_aggregated()}),
        publication_months = reactive({input$pub_mo1}),
        valid_months = reactive({input$valid_mo1}),
        leadtimes =reactive({get_slider_values(input = input,
                                               publication_months = input$pub_mo1,
                                               valid_months = input$valid_mo1)})

      )

    )


  })
}



## To be copied in the UI
# mod_admin_cascade_ui("admin_cascade_1")

## To be copied in the server
# mod_admin_cascade_server("admin_cascade_1")
