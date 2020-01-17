skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
exp_h3_readcodes<-c(".....","H....","X0003","XaBVJ")


setup_dict<-function() {
  context("READ V3 parent code retrieval functions")
  setup_test_dict("NHSReadV3",T)
}

dict<-testthat::setup(setup_dict())


test_that("get_parent_codes returns all READ v3 ancestor codes when default parameters",{
  expect_parent_codes(dict,"H3...",exp_h3_readcodes)
  expect_parent_codes(dict,"H31..",c(".....", "H....", "X0003", "X104H", "X104d", "XaBVJ", "XaDtP"
  ))
  expect_parent_codes(dict,"H32..",c(".....", "H....", "X0003", "XaBVJ"))
})

test_that("get_parent_codes returns filtered READ v3 ancestor codes when immediate_parents flag",{
  expect_parent_codes(dict,"H3...",c("H...."),immediate=T)
  expect_parent_codes(dict,"H31..",c("XaDtP"),immediate=T)
  expect_parent_codes(dict,"H32..",c("H...."),immediate=T)
})


test_that("get_parent_codes returns filtered READ v3 descendent codes when current_only flag",{
  expect_parent_codes(dict,"H3...",c(".....", "H....", "X0003", "XaBVJ"),current=T)
  expect_parent_codes(dict,"H31..",c(".....", "H....", "X0003", "X104H", "X104d", "XaBVJ", "XaDtP"
  ),current=T)
  expect_parent_codes(dict,"H32..",c(".....", "H....", "X0003", "XaBVJ"),current=T)
})

test_that("get_parent_codes returns filtered READ v3 descendent codes when immediate_descendents and current_only flag",{
  expect_parent_codes(dict,"H3...",c("H...."),immediate=T)
  expect_parent_codes(dict,"H31..",c("XaDtP"),immediate=T)
  expect_parent_codes(dict,"H32..",c("H...."),immediate=T)
})

testthat::teardown(cleanup_test_dict(dict))
