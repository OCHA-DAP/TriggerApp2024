any_flagged_by <- function(df, grp_vars=c("window","yr_date"),lgl_flag){
  df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grp_vars))
    ) |>
    dplyr::summarise(
      lgl_flag = any(.data$lgl_flag),
      .groups = "drop"
    )
}
activation_rates_by <-  function(df, grp_vars= c("window"),lgl_flag){
  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(grp_vars)
      )
    ) |>
    dplyr::summarise(
      overall_activation = mean(.data$lgl_flag, na.rm = T),
      overall_rp = 1 / overall_activation
    )
}
