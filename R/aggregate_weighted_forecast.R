
#' aggregate_weighted_forecast
#'
#' @param df data.frame containing selected strata and forecasts data
#'     temporally aggregated by selected publication and valid months
#' @param df_area_loookup data.frame containing selected strata and geographic
#'     areas for weighted spatial aggregation.
#' @param analysis_level `character` vector of selected analysis level.
#'
#' @return data.frame with `value` column representing `weighted.mean` avg
#'     forecast value
#' @export

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


#' calc_weights
#'
#' @param df data.frame containing selected strata and forecasts data
#'     temporally aggregated by selected publication and valid months
#' @param df_area_loookup data.frame containing selected strata and geographic
#'     areas for weighted spatial aggregation.
#' @param analysis_level `character` vector of selected analysis level.
#'
#' @return data.frame with `pct_weight` column
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
