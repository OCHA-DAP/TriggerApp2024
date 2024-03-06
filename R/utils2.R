
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
    dplyr::left_join(df_joint_activation_rates, by =analysis_level)
  return(
    list(
      thresholds= df_thresholds,
      historical_classified =df_historical_classified,
      yearly_flags_lgl = df_yearly_activation_lgl
    )
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
