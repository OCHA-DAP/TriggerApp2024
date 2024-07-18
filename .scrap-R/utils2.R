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
