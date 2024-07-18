#' same as below just df is already aggregated
#' @param df data.frame containing selected strata and forecasts data
#'     temporally aggregated by selected publication and valid months
#' @param leadtimes selected leadtime values from leadtime sliders
#'     returned from get_slider_values()  function.
#' @param analysis_level `character` vector of selected analysis level.
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
    # i think the new group_by approach might be cleaner and more robust than this original one...
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





#' threshold_values
#' @description
#' Take return period values convert to quantiles and find quantile based
#' threshold on selected strata.
#' @param slider_rps list of selected slider return period values.
#'
#' @return data.frame with theshold values for each strata and leadtime
#'     return period combination
#' @export
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



#' gt_style_thresh_table
#'
#' convenience function to stylize `{gt}` threshold table
#' @param gt_ob gt table class object
#' @param table_type `character` option to specify if table to be stylized is
#'     the "overall_activation" or "strata" level table
#'
#' @return gt table object with stylings set
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
        label = "Rainfall threshold (mm) by leadtime",
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

