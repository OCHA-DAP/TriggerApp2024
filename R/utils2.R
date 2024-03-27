#' get_spatial_filter_keys
#'
#' @param adm0_input
#' @param adm1_input
#' @param adm2_input
#' @param adm3_input
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' get_spatial_filter_keys(adm0_input = "a",
#'                         adm1_input = "b",
#'                         adm2_input = "c",
#'                         adm3_input = NULL)

# get_spatial_filter_keys <-  function(adm0_input,
#                                      adm1_input,
#                                      adm2_input,
#                                      adm3_input,
#                                      analysis_level){
#   # could probably combine 2 lists into 1 named list... let's just test this first
#   filter_drill_downs <- list(
#     adm3_pcode=adm3_input,
#     adm2_pcode=adm2_input,
#     adm1_pcode=adm1_input,
#     adm0_pcode= adm0_input
#   )
#
#   filter_drill_down_columns <-  c("adm3_pcode",
#                                   "adm2_pcode",
#                                   "adm1_pcode",
#                                   "adm0_pcode")
#   idx_first_non_null <- which(purrr::map_lgl(filter_drill_downs, ~!is.null(.x)))[1]
#
#   filter_value <-  filter_drill_downs[[idx_first_non_null]]
#   filter_col <-  filter_drill_down_columns[idx_first_non_null]
#   return(
#     list(
#       name=filter_col,
#       value=filter_value
#     )
#
#   )
# }
get_spatial_filter_keys <-  function(adm0_input,
                                     adm1_input,
                                     adm2_input,
                                     adm3_input,
                                     analysis_level){
  # could probably combine 2 lists into 1 named list... let's just test this first
  filter_drill_downs <- list(
    adm3_pcode=adm3_input,
    adm2_pcode=adm2_input,
    adm1_pcode=adm1_input,
    adm0_pcode= adm0_input
  )

  filter_drill_down_columns <-  c("adm3_pcode",
                                  "adm2_pcode",
                                  "adm1_pcode",
                                  "adm0_pcode")
  idx_first_non_null <- which(purrr::map_lgl(filter_drill_downs, ~!is.null(.x)))[1]

  if(
    filter_drill_down_columns[idx_first_non_null-1]==analysis_level #&
    # is.null(filter_drill_downs[[idx_first_non_null]])
    ){
    idx_first_non_null <- idx_first_non_null+1
  }

  filter_value <-  filter_drill_downs[[idx_first_non_null]]
  filter_col <-  filter_drill_down_columns[idx_first_non_null]
  return(
    list(
      name=filter_col,
      value=filter_value
    )

  )
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
    dplyr::inner_join(
      thresh_table_long
    ) |>
    dplyr::mutate(
      lgl_flag = value<q_ind
    )
  return(df_classified)
}
#' same as below just df is already aggregated
run_thresholding2 <-  function(df,
                              leadtimes,
                              analysis_level
){





# create percentile threshold table
  df_thresholds <- threshold_values(
    df= df,
    slider_rps =leadtimes
  )

# classify each record
  df_historical_classified <-  classify_historical(
    df = df,
    thresh_table = df_thresholds
  )

  # based on updated threshold -- classify again to see if any activation occured each year
  df_yearly_activation_lgl <- df_historical_classified |>
    # i think the new group_by apporach might be cleaner and more robust than this original one...
    # dplyr::group_by(!!!rlang::syms(l_inputs$analysis_level()), yr_date) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),
      yr_date) |>
    dplyr::summarise(
      lgl_flag = any(lgl_flag),
      .groups = "drop_last"
    )

  # calculate activation rate across strata
  df_joint_activation_rates <- df_yearly_activation_lgl |>
    dplyr::summarise(
      overall_activation = mean(lgl_flag, na.rm = T),
      overall_rp = 1 / overall_activation
    )

  df_thresholds <- df_thresholds |>
    dplyr::left_join(df_joint_activation_rates)

  ret <- list(
    thresholds= df_thresholds,
    historical_classified =df_historical_classified,
    yearly_flags_lgl = df_yearly_activation_lgl
  )

  num_strata <- length(unique(df[[analysis_level]]))

  if(num_strata>1){
    # if(
    #   length(unique(df_summarised[[analysis_level]]))
    #   <4){
    #   browser()
    # }
    df_summarised_combined <- aggregate_weighted_forecast(df = df,
                                                          df_area_loookup = df_area_lookup,
                                                          analysis_level = analysis_level)
    df_thresholds_combined <-  threshold_values(
      df= df_summarised_combined,
      slider_rps =leadtimes
    )
    df_historical_classified_combined <-  classify_historical(
      df = df_summarised_combined,
      thresh_table = df_thresholds_combined
    )
    df_yearly_activation_lgl_combined <- df_historical_classified_combined |>
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of(
            dplyr::matches("adm\\d_[pe]|adm_combined_[pe]"))),
        yr_date) |>
      dplyr::summarise(
        lgl_flag = any(lgl_flag),
        .groups = "drop_last"
      )
    df_joint_activation_rates_combined <- df_yearly_activation_lgl_combined |>
      dplyr::summarise(
        overall_activation = mean(lgl_flag, na.rm = T),
        overall_rp = 1 / overall_activation
      )
    df_thresholds_combined <- df_thresholds_combined |>
      dplyr::left_join(df_joint_activation_rates_combined)

    ret_combined <- list(
      thresholds_combined= df_thresholds_combined,
      historical_classified_combined =df_historical_classified_combined,
      yearly_flags_lgl_combined = df_yearly_activation_lgl_combined
    )
    ret <- list(ret,ret_combined) |> purrr::flatten()
  }
  return(ret)


}
run_thresholding <-  function(df,
                              valid_months,
                              leadtimes,
                              analysis_level
){


  df_summarised <- summarise_forecast_temporal2(
    df = df,
    valid_month_arg = valid_months
  )

  df_thresholds <- threshold_values(
    df= df_summarised,
    slider_rps =leadtimes
  )


  df_historical_classified <-  classify_historical(
    df = df_summarised,
    thresh_table = df_thresholds
  )

  df_yearly_activation_lgl <- df_historical_classified |>
    # i think the new group_by apporach might be cleaner and more robust than this original one...
    # dplyr::group_by(!!!rlang::syms(l_inputs$analysis_level()), yr_date) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),
      yr_date) |>
    dplyr::summarise(
      lgl_flag = any(lgl_flag),
      .groups = "drop_last"
    )

  df_joint_activation_rates <- df_yearly_activation_lgl |>
    dplyr::summarise(
      overall_activation = mean(lgl_flag, na.rm = T),
      overall_rp = 1 / overall_activation
    )
  df_thresholds <- df_thresholds |>
    dplyr::left_join(df_joint_activation_rates)

  ret <- list(
    thresholds= df_thresholds,
    historical_classified =df_historical_classified,
    yearly_flags_lgl = df_yearly_activation_lgl
  )

  num_strata <- length(unique(df_summarised[[analysis_level]]))

  if(num_strata>1){
    # if(
    #   length(unique(df_summarised[[analysis_level]]))
    #   <4){
    #   browser()
    # }
    df_summarised_combined <- aggregate_weighted_forecast(df = df_summarised,
                                                          df_area_loookup = df_area_lookup,
                                                          analysis_level = analysis_level)
    df_thresholds_combined <-  threshold_values(
      df= df_summarised_combined,
      slider_rps =leadtimes
    )
    df_historical_classified_combined <-  classify_historical(
      df = df_summarised_combined,
      thresh_table = df_thresholds_combined
    )
    df_yearly_activation_lgl_combined <- df_historical_classified_combined |>
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of(
            dplyr::matches("adm\\d_[pe]|adm_combined_[pe]"))),
        yr_date) |>
      dplyr::summarise(
        lgl_flag = any(lgl_flag),
        .groups = "drop_last"
      )
    df_joint_activation_rates_combined <- df_yearly_activation_lgl_combined |>
      dplyr::summarise(
        overall_activation = mean(lgl_flag, na.rm = T),
        overall_rp = 1 / overall_activation
      )
    df_thresholds_combined <- df_thresholds_combined |>
      dplyr::left_join(df_joint_activation_rates_combined)

    ret_combined <- list(
      thresholds_combined= df_thresholds_combined,
      historical_classified_combined =df_historical_classified_combined,
      yearly_flags_lgl_combined = df_yearly_activation_lgl_combined
    )
    ret <- list(ret,ret_combined) |> purrr::flatten()
  }
  return(ret)


}
#' Title
#'
#' @param df
#' @param analysis_level
#'
#' @return
#' @export
#'
#' @examples

calc_weights <- function(df,
                         df_area_lookup=df_area_lookup,
                         analysis_level){

  #' silly string wrangling which should be removed by refactoring earlier
  # analysis_level = "adm1_pcode"
  analysis_level_id <-  stringr::str_remove(analysis_level,"_pcode")

  # just subsets to correct dataset
  df_area_subset <- df_area_lookup |>
    dplyr::filter(
      admin_level ==analysis_level_id
    ) |>
    dplyr::rename_with(
      .cols= c("adm_en","adm_pcode"),
      .fn = function(nm_tmp) stringr::str_replace(nm_tmp,"adm",analysis_level_id)
    )
  df_weights = df_area_subset |>
    dplyr::filter(
      !!rlang::sym(analysis_level) %in% unique(df[[analysis_level]])
    ) |>
    dplyr::mutate(
      pct_wt = area/sum(area)
    )
  return(df_weights)


}

aggregate_weighted_forecast <- function(
    df,
    df_area_loookup,
    analysis_level
){
  df_weights <-  calc_weights(df = df,
                              df_area_lookup = df_area_lookup,
                              analysis_level = analysis_level)
  analysis_level_code <- analysis_level
  analysis_level_en <- stringr::str_replace(analysis_level_code,"pcode","en")



  df |>
    dplyr::left_join(
      df_weights
    ) |>
    dplyr::group_by(
      yr_date,pub_date, lt
    ) |>
    # add adm1_en + adm1_pc
    dplyr::summarise(
      adm_combined_pcode = list(!!rlang::sym(analysis_level_code)),
      adm_combined_en = list(!!rlang::sym(analysis_level_en)),
      value = weighted.mean(value,w=pct_wt)
    )
}



#' Title
#'
#' @param df `data.frame` containing the seasonal/window sum for each leadtime and strata ... df_summarised <- summarise_forecast_temporal2(df, valid_month_arg)
#'
#' @return
#' @export
#'
#' @examples
union_forecast_to_strata <- function(df,df_area,analysis_level){
  # once the seasonal sums are calculated per strata we
  # using the area of each strata we can calculate the average forecast rainfall over
  # all strata with a weighted mean?

  # therefore, we need area added on to data-set... question of performance... is it faster to have an area lookup table that is joined at some stage
  # or have areas from the beginning. Will go with adding on from the beginning as a step in data-raw.....hmmm on the other hand.... i see it
  # highly likely that we will want a map at some point... so should i just go w/ the other approach and build in the spatial files as inputs?
  input.sel_adm1 <-  c("ET14", "ET02", "ET03")


  # df_sel_adm <-
  df <- ldf$adm1 |>
    dplyr::filter(
      if(!is.null(input.sel_adm1)) adm1_pcode %in% input.sel_adm1 else TRUE,
    ) |>
    # separating this filter for trouble shooting. Should be able to combine
    dplyr::filter(
      pub_month %in% c(2,3,4,5),
      valid_month %in% c(5,6,7)
    )
  # debugonce(summarise_forecast_temporal2)
  df_summarised <- summarise_forecast_temporal2(
    df = df,
    valid_month_arg = c(5,6,7)
  )





  # add weights

  # analysis_level_id <- "adm1"

  df_weights <- df_area |>
    subset_area_lookup(
      analysis_level = analysis_level_id
    ) |>
    dplyr::filter(
      !!rlang::sym(analysis_level) %in% unique(df_summarised$adm1_pcode)
    ) |>
    dplyr::mutate(
      pct_wt = area/sum(area)
    )
  df_summarised |>
    dplyr::left_join(
      df_weights
    ) |>
    dplyr::group_by(
      yr_date,pub_date, lt
    ) |>
    summarise(
      adm_codes_combined = paste0(adm1_pcode,collapse = ", "),
      adm_labels_combined = paste0(adm1_en,collapse = ", "),
      value = weighted.mean(value,w=pct_wt)
    )

  df |>
    dplyr::left_join(
      df_area_sub
    )
  dplyr::group_by(
    yr_date,
    pub_date,
    lt
  ) |>
    dplyr::summarise(
      value = weighted.mean()
    )







}

summarise_forecast_temporal_new <- function(df,
                                           publication_month,
                                           valid_month_arg){
  valid_month_arg_values <- as.numeric(valid_month_arg)
  df |>
    dplyr::group_by(
      pub_date,
    dplyr::across(
      dplyr::any_of(
        dplyr::matches("adm\\d_[pe]"))),

  ) |>
    dplyr::filter(all(valid_month_arg %in% valid_month),
           valid_month %in% valid_month_arg) |>
    # non-consequential arrange just makes trouble shooting nicer.
    # dplyr::arrange(
    #     dplyr::matches("adm\\d_[pe]"), pub_date, lt
    # ) |>
    dplyr::group_by(
      pub_date,
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),

    ) |>
    dplyr::mutate(
      count = length(unique(lt))
    ) |>
    dplyr::summarise(
      value = sum(value),
      # **min() - BECAUSE**  for MJJA (5,6,7,8) at each pub_date we have a set of leadtimes
      # for EXAMPLE in March we have the following leadtimes 2
      # 2 : March + 2 = May,
      # 3 : March + 3 = June,
      # 4 : March + 4 = July
      # 5:  March + 5 = Aug
      # Therefore when we sum those leadtime precip values we take min() of lt integer so we get the leadtime to first month being aggregated
      lt = min(lt),
      .groups = "drop"
    ) |>
      dplyr::mutate(
        yr_date = lubridate::floor_date(pub_date, "year")
      )
}



# attempting to deprecate:
# filter_aoi, aggregate_forecast, summarise_temporal
# with new modular approach....
# should now be able to just run a new version of summarise temporal
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
summarise_forecast_temporal2 <- function(df,
                                         # publication_month,
                                         valid_month_arg){
  valid_month_arg_values <- as.numeric(valid_month_arg)

  df_grouped <- df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),
      pub_date
    )

  # previously included, but should be taken care of
  # dplyr::filter(
  #   pub_month %in% publication_month,
  #   valid_month %in% valid_month_arg_values # i don't like this equality
  # )

  df_filt_valid <- df_grouped |>
    # dplyr::group_by(pub_date) |>
    dplyr::filter(
      all(valid_month %in% valid_month_arg_values)
    )

  df_filt_valid |>
    dplyr::summarise(
      # sum the rainfall for each pub date (across lts)
      value = sum(value),

      # grab min lt w/ each pub date.
      lt= min(lt),
      .groups = "drop"

    ) |>
    dplyr::mutate(
      yr_date = lubridate::floor_date(pub_date, "year")
    )
}



#' Title
#'
#' @param slider_rps
#'
#' @return
#' @export
#'
#' @examples
threshold_values <- function(df,slider_rps){
  l_quantile_thresholds <- slider_rps |>
    purrr::imap(
      \(rp,nm){
        q_rp <- 1/as.numeric(rp)
        df |>
          dplyr::filter(
            lt==nm
          ) |>
          dplyr::group_by(
            dplyr::across(
              dplyr::any_of(matches("^adm\\d_[ep]|^adm_combined_[ep]"))
            )
          ) |>
          dplyr::reframe(
            !!rlang::sym(as.character(nm)) := quantile(value,q_rp)
          )
      }
    )
  purrr::reduce(l_quantile_thresholds,dplyr::left_join)


}



gt_style_thresh_table <- function(gt_ob,table_type){
  gt_num_formatted <- gt_ob |>
    gt::fmt_percent(columns = "overall_activation") |>
    gt::fmt_number(
      columns = c(dplyr::any_of(c("0", "1", "2", "3", "4", "5", "6")), "overall_rp"),
      decimals = 1
    ) |>
    gt::cols_hide(columns = ends_with("_pcode"))

  if(table_type == "strata"){
    ret_gt <- gt_num_formatted |>
      gt::tab_spanner(
        columns = dplyr::any_of(as.character(c(0:6))),
        label = "Thresholds (mm) for different\nleadtimes based on selected RPs",
      ) |>
      gt::tab_spanner(
        columns = dplyr::ends_with("_en"),
        label = "Geography"
      ) |>
      gt::tab_spanner(
        columns = dplyr::starts_with("overall_"),
        label = "Probability of activation in any leadtime"
      )
  }
  if(table_type=="combined"){
    ret_gt <- gt_num_formatted |>
      gt::tab_options(column_labels.hidden = TRUE)
  }
  return(ret_gt)
}



admin_choices <- function(df,values, labels, sel_adm){
  parent_admin_level <- readr::parse_number(values)-1
  parent_admin_pcode_val <-  paste0("adm",parent_admin_level,"_pcode")
  df_choices <- df |>
    dplyr::filter(
      parent_admin_level %in% sel_adm
    ) |>
    dplyr::distinct(df,.data[[c(values)]],.data[[labels]])
  # df_choices <- dplyr::distinct(df,.data[[c(values)]],.data[[labels]])
  ret <- rlang::set_names(df_choices[[values]],df_choices[[labels]])
  return(ret)
}
