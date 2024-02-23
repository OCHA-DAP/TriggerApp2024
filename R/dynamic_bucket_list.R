


#' dynamic_bucket_list
#' @description
#' very clunky code to generate dynamic number of buckets for drag and drop.
#' Spent a long time trying to get it work with purrr which shoud have worked,
#' but I found myself in the depths of trying to debug rlang functions so I gave up.
#' purrr outputs were nearly working, but when passed to rlang::list2() produced and empty [[1]]
#' at the top that I couldn't get rid of.
#' @param num_groups
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' dynamic_bucket_list("1")
#' dynamic_bucket_list("3")
#' }
dynamic_bucket_list <- function(num_groups,reservoir){
  switch(
    num_groups,
    "1"=bucket_list1(reservoir=reservoir),
    "2"= bucket_list2(reservoir=reservoir),
    "3"= bucket_list3(reservoir=reservoir),
    "4"= bucket_list4(reservoir=reservoir)
  )
}


bucket_list1 <- function(reservoir){
  sortable::bucket_list(
  header = "Drag the items in any desired bucket",
  group_name = "bucket_list_group",
  orientation = "horizontal",
  sortable::add_rank_list(
    text = "add to Group 1",
    labels = reservoir,
    input_id = "rank_list_1"
  )
)
}

bucket_list2 <- function(reservoir){
  sortable::bucket_list(
  header = "Drag the items in any desired bucket",
  group_name = "bucket_list_group",
  orientation = "horizontal",
  sortable::add_rank_list(
    text = "add to Group 1",
    labels = reservoir,
    input_id = "rank_list_1"
  ),
  sortable::add_rank_list(
    text = "add to Group 2",
    labels = NULL,
    input_id = "rank_list_2"
  )
)
}

bucket_list3 <- function(reservoir){
  sortable::bucket_list(
  header = "Drag the items in any desired bucket",
  group_name = "bucket_list_group",
  orientation = "horizontal",
  sortable::add_rank_list(
    text = "add to Group 1",
    labels = reservoir,
    input_id = "rank_list_1"
  ),
  sortable::add_rank_list(
    text = "add to Group 2",
    labels = NULL,
    input_id = "rank_list_2"
  ),
  sortable::add_rank_list(
    text = "add to Group 3",
    labels = NULL,
    input_id = "rank_list_3"
  )
)
}


bucket_list4 <- function(reservoir){
  sortable::bucket_list(
  header = "Drag the items in any desired bucket",
  group_name = "bucket_list_group",
  orientation = "horizontal",
  sortable::add_rank_list(
    text = "add to Group 1",
    labels = reservoir,
    input_id = "rank_list_1"
  ),
  sortable::add_rank_list(
    text = "add to Group 2",
    labels = NULL,
    input_id = "rank_list_2"
  ),
  sortable::add_rank_list(
    text = "add to Group 3",
    labels = NULL,
    input_id = "rank_list_3"
  ),
  sortable::add_rank_list(
    text = "add to Group 4",
    labels = NULL,
    input_id = "rank_list_4"
  )
)
}


#
#
# add_many_rank_list <-  function(num_list){
#   rank_lists <- 1:num_list |>
#     purrr::map(
#       \(num_tmp){
#         sortable::add_rank_list(
#         text = paste0("add to Group ",num_tmp),
#         labels = NULL,
#         input_id = paste0("rank_list_",num_tmp)
#         )
#
#       }
#     )
#
#
#   class(rank_lists) <- "add_rank_list"
#   return(rank_lists)
# }
# add_many_rank_list2 <- function(num_list){
#   rl <- list()
#   for(i in seq_along(1:num_list)){
#     rl[[i]] <- sortable::add_rank_list(
#       text = paste0("add to Group ",num_list[i]),
#       labels = NULL,
#       input_id = paste0("rank_list_",num_list[i])
#     )
#   }
#   class(rl) <- "add_rank_list"
#   return(rl)
# }
# seq_along(2)
#
# # debugonce(sortable::bucket_list)
# sortable::bucket_list(
#   header = "Drag the items in any desired bucket",
#   group_name = "bucket_list_group",
#   orientation = "horizontal",
#   sortable::add_rank_list(
#     text = "add to Group 1",
#     labels = NULL,
#     input_id = "rank_list_1"
#   ),
#   sortable::add_rank_list(
#     text = "add to Group 2",
#     labels = NULL,
#     input_id = "rank_list_2"
#   )
#
#
# )
#
#
# debugonce(sortable::bucket_list)
#   sortable::bucket_list(
#       header = "Drag the items in any desired bucket",
#       group_name = "bucket_list_group",
#       orientation = "horizontal",
#       add_many_rank_list(num_list = 2)
#     )
#
#   sortable::bucket_list(
#       header = "Drag the items in any desired bucket",
#       group_name = "bucket_list_group",
#       orientation = "horizontal",
#       rlang::list2(add_many_rank_list2(num_list = 2) )[[1]]
#     )
#
#   bla <- rlang::list2(add_many_rank_list2(num_list = 2) )
#   rlang::list2(purrr::flatten(bla))
#   purrr::discard(bla,rlang::is_empty)
#
#
#
#
#
