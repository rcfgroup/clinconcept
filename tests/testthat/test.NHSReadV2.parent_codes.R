skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")

context("READ V2 parent code retrieval function")

dict<-testthat::setup(setup_test_dict("NHSReadV2",T))

test_that("get_parent_codes returns all READ v2 ancestor codes when default parameters",{
  expect_parent_codes(dict,"H3...",c("H...."))
  expect_parent_codes(dict,"H31..",c("H....", "H3..."))
  expect_parent_codes(dict,"H32..",c("H....", "H3..."))
})

test_that("get_parent_codes returns filtered READ v3 ancestor codes when immediate_parents flag",{
  expect_parent_codes(dict,"H3...",c("H...."),immediate=T)
  expect_parent_codes(dict,"H31..",c("H3..."),immediate=T)
  expect_parent_codes(dict,"H32..",c("H3..."),immediate=T)
})

testthat::teardown(cleanup_test_dict(dict))
