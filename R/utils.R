paste_varselects <-  function(l){
  glue::glue_collapse(l,sep = ",")
}

#' load_df_forecast_parquets
#' @description
#' convenience function to load appropriate dataset for app development
#' Does need some work.
#' @return `data.frame` with forecast in format required by app.

#' @examples \dontrun{
#' load_df_forecast(dataset= "mars_eth")
#' }
#' @export
load_df_forecast_parquets <-  function(dataset_dir = ".data-scrap/orig_external_data" ,dataset="mars_lac"){


  if(dataset=="mars_lac"){
    ret <- arrow::read_parquet(
      file.path(
        dataset_dir,
        "df_mars_historical.parquet"
      )
    )

  }
  if(dataset=="mars_eth"){
    eth_files <- stringr::str_subset(list.files('data'),pattern = "eth")
    fp_eth_files <- file.path(
      dataset_dir,
      eth_files
    )
    adm_level_labels <- stringr::str_extract(eth_files,"adm\\d")

    ret <- purrr::map(
      rlang::set_names(fp_eth_files, adm_level_labels),
      \(fp_tmp){
        arrow::read_parquet(
          fp_tmp
        )
      }
    )

  }
  if(dataset=="combined"){
    comb_files <- stringr::str_subset(list.files(dataset_dir),pattern = "df_mars_zonal")
    fp_comb_files <- file.path(
      dataset_dir,
      comb_files
    )
    adm_level_labels <- stringr::str_extract(fp_comb_files,"adm\\d")

    lgdf_adm0_2 <- purrr::map(
      rlang::set_names(fp_comb_files, adm_level_labels),
      \(fp_tmp){
        arrow::read_parquet(
          fp_tmp
        )
      }
    )
    lgdf_adm0_2$adm3 <- arrow::read_parquet(
      file.path(dataset_dir,
                "df_eth_mars_zonal_adm3.parquet")
    )
    ret <- lgdf_adm0_2
  }
  return(ret)
}
