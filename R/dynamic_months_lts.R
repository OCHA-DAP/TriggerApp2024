
#' get_slider_values
#' @description
#' grab user-defined inputs from sliders. Can only really be used in `reactive` context
#' @param input shiny input object
#' @param publication_months `numeric` vector of publication months
#' @param valid_months `numeric` vector of valid months
#'
#' @return list of numeric slider values
#' @export
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

#' available_lts
#'
#' @param publication_months vector of integer publication months
#' @param valid_months vector of integer valid months
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
#' available_lts(publication_months= c(10,12,1), valid_months=c(12,1,2,3))
#' available_lts(publication_months = c(11,12,1), valid_months = c(1,2,3,4))
#'
#' # this is wrong!
#' pub_mos_issue <- c(1,2,10,11,12)
#' valid_mos_issue <- c(2,3,4)
#' available_lts(publication_months = pub_mos_issue, valid_months = valid_mos_issue)

#' pub_mo_issue_fixed <- c(10,11,12,1,2)
#' available_lts(publication_months = pub_mo_issue_fixed, valid_months = valid_mos_issue)
#'
#' # lets seee w/ circular sort
#' available_lts(publication_months = circular_sort(pub_mos_issue), valid_months = circular_sort(valid_mos_issue))

#'
#'
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
  valid_interval <- find_month_range(valid_months)
  pub_interval <- find_month_range(publication_months)
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
#' find_pub_mos(valid_months = c(12,1,2,3))
#' find_pub_mos(valid_months= c())
#' }
find_pub_mos <- function(valid_months){
  list_pub_mos <- load_pub_mo_list() # default to lt 6.... could include param...
  valid_interval <- find_month_range(valid_months)
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
#' find_month_range(m = c(11,12,1,2,3,4,5))
#' find_month_range(m = sort(c(11,12,1,2,3)))
#' find_month_range(m = sort(c(12,1,2,3)))
#' find_month_range(m = c(4,6,7))
#' find_month_range(m = c(10,11,12,1))
#' find_month_range(m = c(11,12,1,2,3))
#' find_month_range(m = c(1,2,3,11,12))
#' }

find_month_range <- function(m){
  m <- as.numeric(m)
  diff_lag <- (m-dplyr::lag(m))


  idx_switch <- which(diff_lag>1)
  if(length(idx_switch)==0){
    max_m <- m[length(m)]
    min_m <- m[1]
  }
  if(length(idx_switch)>0){
    idx_sort <- c(idx_switch:length(m),1:(idx_switch-1))
    m_sorted <- m[idx_sort]
    max_m <- m_sorted[length(m_sorted)]
    min_m <- m_sorted[1]
  }
  return(
    list(earliest = min_m, latest = max_m)
  )
}


#' load_valid_mo_options
#' @param valid_months `integer` containeing valid month sequence
#' @export
load_valid_mo_options <- function(valid_months){
  # Define the number of rows and columns
  n_rows <- 12
  n_cols <- 6

  # Create the matrix with the desired pattern
  matrix <- matrix(nrow = n_rows, ncol = n_cols)

  # Fill the matrix using modular arithmetic
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      matrix[i, j] <- ((i + j - 2) %% n_rows) + 1
    }
  }
  filtered_df <- data.frame(matrix) |>
    dplyr::rowwise() |>
    dplyr::filter(
      contains_sequence(
        dplyr::c_across(everything()), seq =valid_months )
    ) |>
    dplyr::ungroup()

  filtered_df |>
    unlist() |>
    unique()
}


contains_sequence <- function(row, seq) {
  for (i in 1:(length(row) - length(seq) + 1)) {
    if (all(row[i:(i + length(seq) - 1)] == seq)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

