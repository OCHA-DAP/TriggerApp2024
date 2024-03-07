# write the tests right here:
test_that("get_spatial_filter_keys", {  # this is a test block
 expect_equal(
   get_spatial_filter_keys(adm0_input = "a",
                           adm1_input = "b",
                           adm2_input = "c",
                           adm3_input = "d"),  # this is a test
               list(filter_col = "adm3_pcode", filter_value = "d"))  # this is the expected result
 expect_equal(
   get_spatial_filter_keys(adm0_input = "a",
                           adm1_input = c("b","c","d"),
                           adm2_input = "c",
                           adm3_input = "d"),  # this is a test
               list(filter_col = "adm3_pcode", filter_value = "d"))  # this is the expected result
 expect_equal(
   get_spatial_filter_keys(adm0_input = "a",
                           adm1_input = c("b","c","d"),
                           adm2_input = "c",
                           adm3_input = NULL),  # this is a test
               list(filter_col = "adm2_pcode", filter_value = "c"))  # this is the expected result
})
