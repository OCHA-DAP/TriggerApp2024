
#
# adm_lookup <-  function(list_df=ldf, adm_level){
#   adm3_distinct<- list_df[["adm3"]]|>
#     dplyr::select(
#       dplyr::matches("adm\\d_[ep]"),area
#     ) |>
#     dplyr::distinct()
# }
#
# adm2_lookup <-  adm3_distinct |>
#   dplyr::group_by(
#     adm1_en, adm1_pcode, adm2_en,adm2_pcode
#   ) |>
#   dplyr::summarise(
#      area = sum(area, na.rm = TRUE),
#      .groups="drop"
#   )


#' Title
#'
#' @param df
#' @param number_groups
#' @param input
#'
#' @return
#' @export
#'
#' @examples
aggregate_to_strata <- function(list_df,
                                analysis_level,
                                # admin_pcode_values,
                                number_groups,
                                input){
  # analysis_level <- "adm2_pcode"
  df_name <- stringr::str_extract(analysis_level,"adm\\d")

  df_sel <- list_df[[df_name]]

  # df_aoi <- filter_aoi(list_df = ldf,
  #            analysis_level =analysis_level ,
  #            admin_pcode_name = analysis_level,
  #            admin_pcode_values =admin_pcode_values
  #            )

  draggable_ids <- paste0("rank_list_",1:number_groups)
  # if(length(draggable_ids)>1){
  # map through each bucket id and add conditional column
  df_strata_defined <- draggable_ids |>
    purrr::map(
      \(bucket_id_tmp){
        df_sel |>
          dplyr::mutate(
            !!sym(bucket_id_tmp) := dplyr::if_else(!!rlang::sym(analysis_level) %in% input[[bucket_id_tmp]],bucket_id_tmp,NA_character_),
          )
      }
    ) |>
    purrr::reduce(dplyr::left_join) |>
    tidyr::unite(
      "strata" ,
      draggable_ids,
      remove= T,
      na.rm = T
    ) |>
    dplyr::filter(!is.na(strata))

  df_strata_defined |>
    dplyr::group_by(strata,pub_date, valid_date, pub_month, valid_month,lt) |>
    dplyr::summarise(
      # will move this to a weighted.mean once proof of concept is working
      value = mean(value,na.rm=T),.groups="drop"
    ) |>
    # just temporary
    dplyr::count(strata)

}
