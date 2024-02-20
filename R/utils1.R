#' load_df_forecast
#'
#' @return
#' @export
#'
#' @examples
load_df_forecast <-  function(dataset="mars_lac"){


  if(dataset=="mars_lac"){
    arrow::read_parquet("data/df_mars_historical.parquet")
  }
  if(dataset=="mars_eth"){
    eth_files <- stringr::str_subset(list.files('data'),pattern = "eth")
    fp_eth_files <- file.path(
      "data",
      eth_files
    )
    adm_level_labels <- stringr::str_extract(eth_files,"adm\\d")

    purrr::map(
      rlang::set_names(fp_eth_files, adm_level_labels),
      \(fp_tmp){
        arrow::read_parquet(
         fp_tmp
        )
      }

    )

  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
classify_df_seasons <-  function(valid_months, pub_months){

}


#' Title
#'
#' @param zones
#'
#' @return
#' @export
#'
#' @examples
filter_forecast_spatial <-  function(df= forecast_data,
                                     strata_column, strata_value){

  df |>
    dplyr::filter(
      !!sym(strata_column) %in% strata_value
    )

}

#' Title
#'
#' @param input
#' @param publication_months
#' @param valid_months
#'
#' @return
#' @export
#'
#' @examples
get_slider_values <-  function(input,
                               publication_months,
                               valid_months){

  lt_id_tags = adjustable_leadtimes(
    publication_months = publication_months,
    valid_months= valid_months) |>
    sort()

  purrr::set_names(lt_id_tags,lt_id_tags) |>
    purrr::map(\(lt){
      input[[paste0("slider_", lt)]] %||% 4
    })
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
# aggregate_forecast <- function(){
# }


#' Title
#'
#' @param df
#' @param rp
#' @param groups
#'
#' @return
#' @export
#'
#' @examples

# calc_quantile_by_groups <-  function(df,rp,groups){
#   q_rp <- 1/rp
#   df |>
#     dplyr::group_by(!!rlang::sym(groups)) |>
#     dplyr::reframe(
#       !!rlang::sym(as.character(nm))  := quantile(value,q_rp)
#     )
# }

thresholds_from_sliders <- function(df,
                                   # groups,
                                   input,
                                   publication_months,
                                   valid_months){

  slider_values <- get_slider_values(
    input=input,
    publication_months=publication_months,
    valid_months= valid_months
  )
  # groups_en <- stringr::str_replace(groups, "pcode","en")
  # grouper <- rlang::syms(c(groups, groups_en))


  l_quantile_thresholds <- slider_values |>
    purrr::imap(
      \(rp,nm){
        q_rp <- 1/as.numeric(rp)
        df |>
          dplyr::filter(
            lt==nm
          ) |>
          dplyr::group_by(
            # !!!grouper
            dplyr::across(
              dplyr::any_of(matches("adm\\d_[ep]"))
              )
            ) |>
          dplyr::reframe(
            !!rlang::sym(as.character(nm)) := quantile(value,q_rp)
          )
      }
    )
  purrr::reduce(l_quantile_thresholds,dplyr::left_join)


}

classify_historical <- function(
    df,
    thresh_table
){

  thresh_table_long <- thresh_table |>
    tidyr::pivot_longer(
      cols= dplyr::matches("\\b[0-6]\\b"),
      names_to ="lt",
      values_to = "q_ind"
    ) |>
    dplyr::mutate(
      lt = as.numeric(lt)
    )

  df_classified <- df |>
    dplyr::left_join(
      thresh_table_long
    ) |>
    dplyr::mutate(
      lgl_flag = value<q_ind
    )
  return(df_classified)
}

# df_win_long <- df_win1() %>%
#   select(-pub_date)

# ldf_rps <- l_rps %>%
#   imap(
#     \(rp,nm){
#       q_rp <- 1/as.numeric(rp)
#       df_win_long %>%
#         filter(
#           lt==nm
#         ) %>%
#         group_by(adm0_es) %>%
#         reframe(
#           !!sym(as.character(nm))  := quantile(mm,q_rp)
#         )




# plot_historical <-  function(){
#
# }


find_pub_mos <-  function(x=c(5,6,7,8)){
  # browser()
  x <- as.numeric(unname(x))
  avail_mo_list <- load_pub_mo_list(lt=6)
  month_diffs <- diff(c(x, x[1] + 12))

  # Find the index of the maximum difference
  max_diff_index <- which.max(month_diffs)

  # The maximum month is the one following the index with the maximum difference
  max_month <- x[max_diff_index]
  max_month_lab <- lubridate::month(max_month,label=T, abbr=T)
  pub_mos <- avail_mo_list[[max_month_lab]]
  ret <- pub_mos[!(pub_mos %in% x[2:length(x)])]

  # hacky solution to fix the fact that if (for example)
  # we are monitoring:  5,6,7 we want to exclude May
  # but if just monitoring 5 there is nothing to exclude

  if(length(x)==1){
    ret <- pub_mos
  } else{
    ret <- pub_mos[!(pub_mos %in% x[2:length(x)])]
  }
  return(ret)
}

load_pub_mo_list <- function(lt=6){
  rlang::set_names(c(1:12),
            lubridate::month(c(1:12),label=T,abbr=T)
  ) |>
    purrr:::map(\(mo_int){
      start_mo_int <- mo_int-lt
      seq_mo_int <-  start_mo_int:mo_int
      mo_seq<- ifelse(seq_mo_int<=0, seq_mo_int+12, seq_mo_int)
      return(mo_seq)
    }
    )
}


adjustable_leadtimes <-  function(publication_months, valid_months){
  pub_mos <-  as.numeric(publication_months)
  valid_mo_start <- min(as.numeric(valid_months))
  return(valid_mo_start- pub_mos)
}

#' admin_choices
#'
#' @param list_df
#' @param analysis_level
#' @param admin_selection
#'
#' @return
#' @export
#' @importFrom rlang sym
#'
#' @examples \dontrun{
#' library(TriggerApp2024)
#' ldf$adm2
#' populate_admin_choices(list_df = ldf,
#'               populate_with_analysis_level = "adm2_pcode",
#'               previous_selection_values = "ET06"
#'               )
#' }
populate_admin_choices <-  function(
    list_df = ldf,
    populate_with_analysis_level = "adm2_pcode",
    previous_selection_values= "ET06"#input.sel_adm1
){
  name_admin_en <- stringr::str_replace(populate_with_analysis_level,"pcode","en") # get english colname
  admin_level_int <-  readr::parse_number(stringr::str_subset(populate_with_analysis_level,"\\d"))
  parent_admin_level_int <- admin_level_int-1

  parent_pcode_name = paste0("adm",parent_admin_level_int,"_pcode")

  if(is.null(previous_selection_values)){
    df_name <- stringr::str_extract(populate_with_analysis_level,"adm\\d")
    df_sel <- list_df[[df_name]]
    ret_df <- df_sel |>
      dplyr::distinct(
        !!sym(populate_with_analysis_level),
        !!sym(name_admin_en)
      )
  }
  if(!is.null(previous_selection_values)){
    ret_df <- filter_aoi(
      list_df = list_df,
      analysis_level = populate_with_analysis_level,
      admin_pcode_name =parent_pcode_name,
      admin_pcode_values = previous_selection_values
    ) |>
      dplyr::distinct(
        !!sym(populate_with_analysis_level),
        !!sym(name_admin_en)
      )
  }
  ret <- rlang::set_names(ret_df[[populate_with_analysis_level]],
                          ret_df[[name_admin_en]])
  return(ret)
}

update_admin_choices <-  function(session,
                                  list_df,
                                  admin_level_choices = "adm2",
                                  parent_selection
                                  ){

  # this doesnt get used anymore, but I want to keep it to remind myself to look for a way
  # to run shiny::updateSelectizeInput directly int this function for a cleaner app
  # issue currently is with the shiny session object

  update_id <- paste0("sel_",admin_level_choices)
  update_en <- paste0(admin_level_choices,"_en")
  update_pcode <-  paste0(admin_level_choices,"_pcode")

  parent_level <- readr::parse_number(stringr::str_extract(admin_level_choices,"\\d"))-1
  parent_pcode <-  paste0("adm",parent_level,"_pcode")

  parent_id <-  paste0("sel_adm",parent_level)

  df_ret <- list_df[[admin_level_choices]] |>
    dplyr::filter(!!rlang::sym(parent_pcode) %in% parent_selection) |>
    dplyr::distinct(!!!rlang::syms(c(update_pcode,update_en)))

  ret <- rlang::set_names(df_ret[[update_pcode]],
                          df_ret[[update_en]])
  return(ret)

}

filter_aoi <-  function(list_df,
                         analysis_level,
                         admin_pcode_name,
                         admin_pcode_values){
  df_name <- stringr::str_extract(analysis_level,"adm\\d")

  df_sel <- list_df[[df_name]]
  df_aoi <- df_sel |>
    dplyr::filter(
      !!sym(admin_pcode_name) %in% admin_pcode_values
    )
  return(df_aoi)
}


aggregate_forecast <- function(
    list_df,
    analysis_level,
    publication_month,
    valid_month,
    admin_pcode_name,
    admin_pcode_values
){


  df_aoi <- filter_aoi(
    list_df= list_df,
    analysis_level = analysis_level ,
    admin_pcode_name=admin_pcode_name,
    admin_pcode_values=admin_pcode_values
  )
  summarise_forecast_temporal(
    df = df_aoi,
    publication_month=publication_month,
    valid_month_arg=valid_month
  )
}

#' summarise_forecast_temporal
#'
#' @param df
#' @param publication_month
#' @param valid_month
#'
#' @return
#' @export
#'
#' @examples
summarise_forecast_temporal <- function(df,
                                     publication_month,
                                     valid_month_arg){
  valid_month_arg_values <- as.numeric(valid_month_arg)

  df_filt1 <- df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),
      pub_date
    ) |>
    dplyr::filter(
      pub_month %in% publication_month,
      valid_month %in% valid_month_arg_values # i don't like this equality
    )

  df_filt2 <- df_filt1 |>
    # dplyr::group_by(pub_date) |>
    dplyr::filter(
      all(valid_month %in% valid_month_arg_values)
    )

  df_filt2 |>
    dplyr::summarise(
      # sum the rainfall for each pub date (across lts)
      value = sum(value),

      # grab min lt w/ each pub date.
      lt= min(lt),
      .groups = "drop"

    ) |>
    dplyr::mutate(
      yr_date = lubridate::floor_date(pub_date, "year")
    )}



admin_ids <-  function(analysis_level,label_only=T){
  label <- stringr::str_replace_all(analysis_level,"pcode","en")
  if(label_only){
    ret <- label
  }
  if(!label_only){
    ret <- c(analysis_level,label)
  }
  return(ret)
}
#' plot_historical
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom ggplot2 aes theme

plot_historical <-  function(df,
                             analysis_level,
                             plot_title
                             ){
  strata_col <- admin_ids(
    analysis_level,
    label_only = T
    )
  df |>
    ggplot2::ggplot(
      aes(x= pub_date,
          y= value,
          color=lgl_flag)
    )+
    ggplot2::geom_point()+
    ggplot2::scale_color_manual(values =c("blue","red"))+
    ggplot2::geom_hline(
      data = df |>
        dplyr::select(
          strata_col,
          lt,
          q_ind
        ) |>
        dplyr::distinct(),
      aes(yintercept = q_ind)
    )+
    ggplot2::scale_x_date(
      date_breaks = "5 year",
      date_labels = "%y"
    )+
    ggplot2::geom_text(
      data= dplyr::filter(df, lgl_flag),
      aes(x=pub_date, y= value*0.85,
          label = format(pub_date,"%y")),
      size= 2
    )+
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle=90),
      legend.position  = "none",
      panel.border  = ggplot2::element_rect(fill=NA, color="lightgrey")
    )+
    ggplot2::facet_grid(
      row=ggplot2::vars(lt),
      cols = ggplot2::vars(!!sym(strata_col))
    )+
    ggplot2::labs(
      x= "Date",
      y= "Total Rainfall (mm)",
      title = plot_title
    )+
    gghdx::theme_hdx()+
    ggplot2::theme(
      legend.position= "none",
      legend.title = ggplot2::element_blank()
    )
}


lookup_rename_gt <-  function(analysis_level){

  list_names <- list(
    overall_activation= "Joint Activation",
    overall_rp = "Joint RP",
    adm0_en = "Country",
    adm1_en = "Region",
    adm2_en = "District",
    adm3_en = "Woreda"
  )
  switch(analysis_level,
         "adm0_pcode" = list_names[1:3],
         "adm1_pcode" = list_names[1:4],
         "adm2_pcode" = list_names[1:5],
         "adm3_pcode" = list_names[1:6])
}

# scrap / cool ideas

# observe({
#
#   analysis_int <- readr::parse_number(stringr::str_extract(analysis_level,"\\d"))
#   analysis_sel_cascading <- c(paste0("sel_adm",0:(analysis_int-1)))
#
#
#   analysis_sel_cascading |>
#     purrr:::map(\(sel_id_tmp){
#       sel_adm_tmp <- input[[sel_id_tmp]]
#       updated_choices <- populate_admin_choices(
#         list_df = ldf,
#         populate_with_analysis_level = input$analysis_level,
#         previous_selection_values = sel_adm_tmp
#       )
#       # d_use <- paste0("sel_",stringr::str_extract(input$analysis_level,"adm\\d"))
#       updateSelectizeInput(session,
#                            inputId = sel_id_tmp,
#                            choices = updated_choices
#       )
#
#     })
#
# })
