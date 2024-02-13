#' load_df_forecast
#'
#' @return
#' @export
#'
#' @examples
load_df_forecast <-  function(dataset="mars"){
  if(dataset=="mars"){
    arrow::read_parquet("data/df_mars_historical.parquet")
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
#' @return
#' @export
#'
#' @examples
filter_forecast_temporal <-  function(valid_months, publication_months){

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
                                     zones
                                     ){

  df |>
    filter(
      adm0_es %in% zones
    )

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
aggregate_forecast <- function(){
}


calculate_quantiles <-  function(){

}

classify_historical <- function(){

}


plot_historical <-  function(){

}
