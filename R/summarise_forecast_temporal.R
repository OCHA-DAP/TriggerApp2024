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
