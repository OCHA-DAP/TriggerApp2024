test_that("Available LTs work", {
  expect_equal(
    label = "Test 1",
    available_lts(
    publication_months= c(12,1,2,3,4,5),
    valid_months=c(5,6)
    ),
  c("5"=12,"4"=1,"3"=2,"2"=3,"1"=4,"0"=5)
  )
  expect_equal(label = "Test 2",
    available_lts(publication_months= c(3,4), valid_months=4),
    c("1"=3,"0"=4)
  )
  expect_equal(
    label = "Testing find_valid_month_interval() with sequential multi year vector",
    find_valid_month_interval(valid_months = c(11,12,1,2,3,4,5)),
    list("earliest"=11,"latest"=5)
  )
  expect_equal(
    label = "Testing find_valid_month_interval() with non-sequential single year",
    find_valid_month_interval(valid_months = c(4,6,7)),
    list("earliest"=4,"latest"=7)
  )
  expect_equal(
    label = "Testing find_valid_month_interval() with non-sequential multi year",
    find_valid_month_interval(valid_months = c(11,12,2,3)),
    list("earliest"=11,"latest"=3)
  )
  expect_equal(
    label = "Testing find_valid_month_interval() with non-sequential multi year - gap first year",
    find_valid_month_interval(valid_months = c(10,12,2,3)),
    list("earliest"=11,"latest"=3)
  )
})


