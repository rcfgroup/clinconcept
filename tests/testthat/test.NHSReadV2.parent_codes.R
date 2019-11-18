library(clinconcept)
library(testthat)
library(readr)
if(!is_sqlite_available()) {
  return(NA)
}

dict<-setup_test_dict("NHSReadV2",F)

context("READ V2 parent code retrieval functions")

test_that("get_parent_codes returns all READ v2 ancestor codes when default parameters",{
  skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
  skip_on_cran()
  expect_parent_codes(dict,"H3...",c("H...."))
  expect_parent_codes(dict,"H31..",c("H....", "H3..."))
  expect_parent_codes(dict,"H32..",c("H....", "H3..."))
})

test_that("get_parent_codes returns filtered READ v3 ancestor codes when immediate_parents flag",{
  skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
  skip_on_cran()
  expect_parent_codes(dict,"H3...",c("H...."),immediate=T)
  expect_parent_codes(dict,"H31..",c("H3..."),immediate=T)
  expect_parent_codes(dict,"H32..",c("H3..."),immediate=T)
})

cc_disconnect(dict)

