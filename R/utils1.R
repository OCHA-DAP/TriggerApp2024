paste_varselects <-  function(l){
  glue::glue_collapse(l,sep = ",")
}

#' load_df_forecast
#' @description
#' convenience function to load appropriate dataset for app development
#' Does need some work.
#' @return `data.frame` with forecast in format required by app.
#' @export
#'
#' @examples \dontrun{
#' load_df_forecast(dataset= "mars_eth")
#' }
load_df_forecast <-  function(dataset="mars_lac"){


  if(dataset=="mars_lac"){
    ret <- arrow::read_parquet("data/df_mars_historical.parquet")
  }
  if(dataset=="mars_eth"){
    eth_files <- stringr::str_subset(list.files('data'),pattern = "eth")
    fp_eth_files <- file.path(
      "data",
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
    comb_files <- stringr::str_subset(list.files('data'),pattern = "df_mars_zonal")
    fp_comb_files <- file.path(
      "data",
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
    lgdf_adm0_2$adm3 <- arrow::read_parquet(file.path("data",
                                                      "df_eth_mars_zonal_adm3.parquet")
    )
    ret <- lgdf_adm0_2
  }
  return(ret)
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
                                     strata_column, strata_value){

  df |>
    dplyr::filter(
      !!sym(strata_column) %in% strata_value
    )

}

#' get_slider_values
#' @description
#' grab user-defined inputs from sliders. Can only really be used in `reactive` context
#' @param input shiny input object
#' @param publication_months `numeric` vector of publication months
#' @param valid_months `numeric` vector of valid months
#'
#' @return list of numeric slider values
#' @export
#'
#' @examples
get_slider_values <-  function(input,
                               publication_months,
                               valid_months){

  lts = available_lts(
    publication_months = publication_months,
    valid_months= valid_months
  )

  lt_id_tags <- names(lts)

  purrr::set_names(lt_id_tags,lt_id_tags) |>
    purrr::map(\(lt){
      input[[paste0("slider_", lt)]] %||% 4
    })
}





#' Title
#'
#' @param df
#' @param rp
#' @param groups
#'
#' @return
#' @export
#'
#' @examples

# calc_quantile_by_groups <-  function(df,rp,groups){
#   q_rp <- 1/rp
#   df |>
#     dplyr::group_by(!!rlang::sym(groups)) |>
#     dplyr::reframe(
#       !!rlang::sym(as.character(nm))  := quantile(value,q_rp)
#     )
# }

thresholds_from_sliders <- function(df,
                                    # groups,
                                    input,
                                    publication_months,
                                    valid_months){

  slider_values <- get_slider_values(
    input=input,
    publication_months=publication_months,
    valid_months= valid_months
  )
  # groups_en <- stringr::str_replace(groups, "pcode","en")
  # grouper <- rlang::syms(c(groups, groups_en))


  l_quantile_thresholds <- slider_values |>
    purrr::imap(
      \(rp,nm){
        q_rp <- 1/as.numeric(rp)
        df |>
          dplyr::filter(
            lt==nm
          ) |>
          dplyr::group_by(
            # !!!grouper
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



# df_win_long <- df_win1() %>%
#   select(-pub_date)

# ldf_rps <- l_rps %>%
#   imap(
#     \(rp,nm){
#       q_rp <- 1/as.numeric(rp)
#       df_win_long %>%
#         filter(
#           lt==nm
#         ) %>%
#         group_by(adm0_es) %>%
#         reframe(
#           !!sym(as.character(nm))  := quantile(mm,q_rp)
#         )




# plot_historical <-  function(){
#
# }


# find_pub_mos <-  function(x=c(5,6,7,8)){
#   # browser()
#   x <- as.numeric(unname(x))
#   avail_mo_list <- load_pub_mo_list(lt=6)
#   month_diffs <- diff(c(x, x[1] + 12))
#
#   # Find the index of the maximum difference
#   max_diff_index <- which.max(month_diffs)
#
#   # The maximum month is the one following the index with the maximum difference
#   max_month <- x[max_diff_index]
#   max_month_lab <- lubridate::month(max_month,label=T, abbr=T)
#   pub_mos <- avail_mo_list[[max_month_lab]]
#   ret <- pub_mos[!(pub_mos %in% x[2:length(x)])]
#
#   # hacky solution to fix the fact that if (for example)
#   # we are monitoring:  5,6,7 we want to exclude May
#   # but if just monitoring 5 there is nothing to exclude
#
#   if(length(x)==1){
#     ret <- pub_mos
#   } else{
#     ret <- pub_mos[!(pub_mos %in% x[2:length(x)])]
#   }
#   return(ret)
# }

#' available_lts
#'
#' @param publication_months
#' @param valid_months
#'
#' @return `numeric` named vector where value represent integer for month and name is the integer of leadtime
#' @export
#'
#' @examples \dontrun{
#' valid_month_ex <- c(5,6)
#' available_lts(publication_months= find_pub_mos(valid_month_ex), valid_months=valid_month_ex)
#' available_lts(publication_months= 5, valid_months=valid_month_ex)
#' available_lts(publication_months= c(3,4), valid_months=4)
#' available_lts(publication_months= c(3), valid_months=4)
#' available_lts(publication_months= c(2), valid_months=4)
#'
#' # what if there is a gap? should this not be allowed somehow or should it be handled
#' available_lts(publication_months= c(2,3),valid_months=c(4,6))
#' valid_mo_ex2 <- c(4,5)
#' all_pub_mo_ex2 <- find_pub_mos(valid_mo_ex2)
#' #> 11,12,1, 2, 3, 4 # looks good
#'  publication_months = all_pub_mo_ex2
#' available_lts(publication_months = c(12,1,2,3), valid_months= valid_mo_ex2)
#' }
available_lts <-  function(publication_months, valid_months=c(5,6)){
  list_pub_mos <- load_pub_mo_list()
  # consider renaming `find_valid_month_interval` for purpose before (pub_mos)
  valid_interval <- find_valid_month_interval(valid_months)
  pub_interval <- find_valid_month_interval(publication_months)
  latest_month_chr <- lubridate::month(valid_interval$latest,label=T,abbr=T)

  # get list of all possible leadtimes months given the latest possible valid month
  all_lts <- list_pub_mos[[latest_month_chr]]

  # name each of these month (integers) with our leadtime integers (0-6)
  all_lts_named <- rlang::set_names(all_lts,(length(all_lts):1)-1)
  lt_idx_start <- which(unname(all_lts_named) == pub_interval$earliest)
  lt_idx_end <- which(unname(all_lts_named) == pub_interval$latest)

  # in a sense the names used here are the correct lts as they represent the number of
  # months to the final valid_month, but i think most people think about in terms of
  # lead time to first valid_month
  ret_lts <- all_lts_named[lt_idx_start:lt_idx_end]

  # shift lt label to fit the above notion
  shift_lt_labels <- valid_interval$latest-valid_interval$earliest
  ret_lts <- rlang::set_names(ret_lts,as.numeric(names(ret_lts))-shift_lt_labels)

  return(ret_lts)

}

#' find_pub_mos
#'
#' @param valid_months `numeric` vector of valid months
#'
#' @return `integer` vector of publication months available based on supplied valid_months
#' @export
#'
#' @examples \dontrun{
#' find_pub_mos(valid_months = c(12,1,2,3,4,5))
#' find_pub_mos(valid_months = c(5,6))
#' }
find_pub_mos <- function(valid_months){
  list_pub_mos <- load_pub_mo_list() # default to lt 6.... could include param...
  valid_interval <- find_valid_month_interval(valid_months)
  latest_month_chr <- lubridate::month(valid_interval$latest,label=T,abbr=T)
  all_pub_mos <- list_pub_mos[[latest_month_chr]]
  idx_pub_cutoff <- which(all_pub_mos==valid_interval$earliest)
  ret_pub_mos <- all_pub_mos[1:idx_pub_cutoff]
  return(ret_pub_mos)
}


load_pub_mo_list <- function(lt=6){
  rlang::set_names(c(1:12),
                   lubridate::month(c(1:12),label=T,abbr=T)
  ) |>
    purrr:::map(\(mo_int){
      start_mo_int <- mo_int-lt
      seq_mo_int <-  start_mo_int:mo_int
      mo_seq<- ifelse(seq_mo_int<=0, seq_mo_int+12, seq_mo_int)
      return(mo_seq)
    }
    )
}


#' find_latest_valid_month
#' @description
#' Correctly find latest month provided by user.
#' Especially useful if valid month window spans the new year Dec-Jan
#' @param valid_months
#'
#' @return `integer` value of latest month
#' @export
#'
#' @examples \dontrun{
#' # should throw these in testthat
#' find_valid_month_interval(valid_months = c(12,1,2,3,4,5))
#' find_valid_month_interval(valid_months = c(11,12,1,2,3,4,5))
#' find_valid_month_interval(valid_months = sort(c(11,12,1,2,3)))
#' find_valid_month_interval(valid_months = sort(c(12,1,2,3)))
#' find_valid_month_interval(valid_months = c(4,6,7))
#' }

find_valid_month_interval <- function(valid_months){
  valid_months <- as.numeric(valid_months)
  diff_lag <- (valid_months-dplyr::lag(valid_months))>0

  # experimenting to try to allow gaps in valid_months ####
  # all_seqs <- load_pub_mo_list(lt = 6)
  # full_seq <- c(1:12)
  # full_seq[min(valid_months):max(valid_months)]
  # c(valid_months, full_seq[!(full_seq %in% valid_months)])
  #
  # if(any(10,11,12 %in% valid_months)& any(1,2,3 %in% valid_months)){
  #
  # }
  ## end experimentation: ####


  idx_switch <- which(diff_lag>1)
  if(length(idx_switch)==0){
    max_month <- valid_months[length(valid_months)]
    min_month <- valid_months[1]
  }
  if(length(idx_switch)>0){
    idx_sort <- c(idx_switch:length(valid_months),1:(idx_switch-1))
    valid_months_sorted <- valid_months[idx_sort]
    max_month <- valid_months_sorted[length(valid_months_sorted)]
    min_month <- valid_months_sorted[1]
  }
  return(
    list(earliest = min_month, latest = max_month)
  )
}



adjustable_leadtimes <-  function(publication_months, valid_months){
  pub_mos <-  as.numeric(publication_months)
  valid_mo_start <- min(as.numeric(valid_months))
  return(valid_mo_start- pub_mos)
}





#' Title
#'
#' @param publication_months
#' @param valid_months
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' valid_months_example <- c(4,5)
#' pub_months_example<- find_pub_mos(x =valid_months_example)
#' adjustable_leadtimes_robust(publication_months = pub_months_example,valid_months = valid_months_example)
#'
#' }
adjustable_leadtimes_robust <-  function(publication_months,valid_months){
  publication_mo <- as.numeric(publication_months)
  valid_mo <- as.numeric(valid_months)
  multi_yr_pub <- publication_months[1]>publication_months[length(publication_months)]
  if(multi_yr_pub){
    dec_idx <- which(!publication_months>dplyr::lag(publication_months))-1
    publication_months[dec_idx] # issue if skip Dec?
    prev_year_mos <- publication_months[1:dec_idx]
    prev_year_dates <- lubridate::as_date(paste0("2020-", formatC(prev_year_mos,
                                                                  digits = 2,
                                                                  flag = "0",
                                                                  width = 2),"-01"))
    next_year_mos <-  publication_months[(dec_idx+1):length(publication_months)]
    next_year_dates <- lubridate::as_date(paste0("2021-", formatC(next_year_mos,
                                                                  digits = 2,
                                                                  flag = "0",
                                                                  width = 2),"-01"))
    all_dates <- c(prev_year_dates,next_year_dates)
    min_pub_date <- min(all_dates)
    # not designed yet for multi yr validation
    min_valid_month <- min(as.numeric(valid_months))
    min_valid_date <- lubridate::as_date(paste0("2021-", formatC(min_valid_month,
                                                                 digits = 2,
                                                                 flag = "0",
                                                                 width = 2),"-01"))
    # this should be improved -- go direct from days to months rather than arith
    lts_days <- (min_valid_date- all_dates)+1
    # for some reaso counting 30 days as 0 months --
    # therefore add 1 to make it recognize as month = 1
    # lts_mos <- as.period(lts_days)%/% months(1)
    lts_mos <- as.numeric(round(lts_days/30))
    # as.period(31,"days")%/% months(1)
  }
  if(!multi_yr_pub){
    mon_mo <-  as.numeric(publication_months)
    valid_start <- min(as.numeric(valid_months))
    lts_mos <- return(valid_start- mon_mo)
  }
  return(lts_mos)
}


#' admin_choices
#'
#' @param list_df
#' @param analysis_level
#' @param admin_selection
#'
#' @return
#' @export
#' @importFrom rlang sym
#'
#' @examples \dontrun{
#' library(TriggerApp2024)
#' ldf$adm2
#' populate_admin_choices(list_df = ldf,
#'               populate_with_analysis_level = "adm2_pcode",
#'               previous_selection_values = "ET06"
#'               )
#' }
populate_admin_choices <-  function(
    list_df = ldf,
    populate_with_analysis_level = "adm2_pcode",
    previous_selection_values= "ET06"#input.sel_adm1
){
  name_admin_en <- stringr::str_replace(populate_with_analysis_level,"pcode","en") # get english colname
  admin_level_int <-  readr::parse_number(stringr::str_subset(populate_with_analysis_level,"\\d"))
  parent_admin_level_int <- admin_level_int-1

  parent_pcode_name = paste0("adm",parent_admin_level_int,"_pcode")

  if(is.null(previous_selection_values)){
    df_name <- stringr::str_extract(populate_with_analysis_level,"adm\\d")
    df_sel <- list_df[[df_name]]
    ret_df <- df_sel |>
      dplyr::distinct(
        !!sym(populate_with_analysis_level),
        !!sym(name_admin_en)
      )
  }
  if(!is.null(previous_selection_values)){
    ret_df <- filter_aoi(
      list_df = list_df,
      analysis_level = populate_with_analysis_level,
      admin_pcode_name =parent_pcode_name,
      admin_pcode_values = previous_selection_values
    ) |>
      dplyr::distinct(
        !!sym(populate_with_analysis_level),
        !!sym(name_admin_en)
      )
  }
  ret <- rlang::set_names(ret_df[[populate_with_analysis_level]],
                          ret_df[[name_admin_en]])
  return(ret)
}


#' ui_update_admin
#'
#' @param session
#' @param input
#' @param list_df
#' @param admin_level_choices
#'
#' @return
#' @export
#'
#' @examples
ui_update_admin <-  function(session=session,
                             input=input,
                             list_df,
                             admin_level_choices = "adm2"

){

  update_id <- paste0("sel_",admin_level_choices)
  update_en <- paste0(admin_level_choices,"_en")
  update_pcode <-  paste0(admin_level_choices,"_pcode")

  parent_level <- readr::parse_number(stringr::str_extract(admin_level_choices,"\\d"))-1
  parent_pcode <-  paste0("adm",parent_level,"_pcode")

  parent_id <-  paste0("sel_adm",parent_level)

  df_ret <- list_df[[admin_level_choices]] |>
    dplyr::filter(!!rlang::sym(parent_pcode) %in% input[[parent_id]]) |>
    dplyr::distinct(!!!rlang::syms(c(update_pcode,update_en)))

  updated_choices <- rlang::set_names(df_ret[[update_pcode]],
                                      df_ret[[update_en]])


  updateSelectizeInput(session,
                       inputId = update_id,
                       choices = updated_choices#,
                       # selected= updated_choices[1:2] # kinda works.
  )



  # return(ret)


}





filter_aoi <-  function(list_df,
                        analysis_level,
                        admin_pcode_name,
                        admin_pcode_values){
  df_name <- stringr::str_extract(analysis_level,"adm\\d")

  df_sel <- list_df[[df_name]]
  df_aoi <- df_sel |>
    dplyr::filter(
      !!sym(admin_pcode_name) %in% admin_pcode_values
    )
  return(df_aoi)
}


aggregate_forecast <- function(
    list_df,
    analysis_level,
    publication_month,
    valid_month,
    admin_pcode_name,
    admin_pcode_values
){


  df_aoi <- filter_aoi(
    list_df= list_df,
    analysis_level = analysis_level ,
    admin_pcode_name=admin_pcode_name,
    admin_pcode_values=admin_pcode_values
  )
  summarise_forecast_temporal(
    df = df_aoi,
    publication_month=publication_month,
    valid_month_arg=valid_month
  )
}

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
summarise_forecast_temporal <- function(df,
                                        publication_month,
                                        valid_month_arg){
  valid_month_arg_values <- as.numeric(valid_month_arg)

  df_filt1 <- df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          dplyr::matches("adm\\d_[pe]"))),
      pub_date
    ) |>
    dplyr::filter(
      pub_month %in% publication_month,
      valid_month %in% valid_month_arg_values # i don't like this equality
    )

  df_filt2 <- df_filt1 |>
    # dplyr::group_by(pub_date) |>
    dplyr::filter(
      all(valid_month %in% valid_month_arg_values)
    )

  df_filt2 |>
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




admin_ids <-  function(analysis_level,label_only=T){
  label <- stringr::str_replace_all(analysis_level,"pcode","en")
  if(label_only){
    ret <- label
  }
  if(!label_only){
    ret <- c(analysis_level,label)
  }
  return(ret)
}
#' plot_historical
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom ggplot2 aes theme

plot_historical <-  function(df,
                             analysis_level,
                             plot_title
){
  strata_col <- admin_ids(
    analysis_level,
    label_only = T
  )
  df |>
    ggplot2::ggplot(
      aes(x= pub_date,
          y= value,
          color=lgl_flag)
    )+
    ggplot2::geom_point()+
    ggplot2::scale_color_manual(values =c("blue","red"))+
    ggplot2::geom_hline(
      data = df |>
        dplyr::select(
          strata_col,
          lt,
          q_ind
        ) |>
        dplyr::distinct(),
      aes(yintercept = q_ind)
    )+
    ggplot2::scale_x_date(
      date_breaks = "5 year",
      date_labels = "%y"
    )+
    ggplot2::geom_text(
      data= dplyr::filter(df, lgl_flag),
      aes(x=pub_date, y= value*0.85,
          label = format(pub_date,"%y")),
      size= 2
    )+
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle=90),
      legend.position  = "none",
      panel.border  = ggplot2::element_rect(fill=NA, color="lightgrey")
    )+
    ggplot2::facet_grid(
      row=ggplot2::vars(lt),
      cols = ggplot2::vars(!!sym(strata_col))
    )+
    ggplot2::labs(
      x= "Date",
      y= "Total Rainfall (mm)",
      title = plot_title
    )+
    gghdx::theme_hdx()+
    ggplot2::theme(
      legend.position= "none",
      legend.title = ggplot2::element_blank()
    )
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

# scrap / cool ideas

# observe({
#
#   analysis_int <- readr::parse_number(stringr::str_extract(analysis_level,"\\d"))
#   analysis_sel_cascading <- c(paste0("sel_adm",0:(analysis_int-1)))
#
#
#   analysis_sel_cascading |>
#     purrr:::map(\(sel_id_tmp){
#       sel_adm_tmp <- input[[sel_id_tmp]]
#       updated_choices <- populate_admin_choices(
#         list_df = ldf,
#         populate_with_analysis_level = input$analysis_level,
#         previous_selection_values = sel_adm_tmp
#       )
#       # d_use <- paste0("sel_",stringr::str_extract(input$analysis_level,"adm\\d"))
#       updateSelectizeInput(session,
#                            inputId = sel_id_tmp,
#                            choices = updated_choices
#       )
#
#     })
#
# })
