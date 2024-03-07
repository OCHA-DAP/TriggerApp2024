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
})
