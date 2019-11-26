skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")

dict<-setup_test_dict("NHSICD10",F)

context("ICD10 parent code retrieval functions")

test_that("get_parent_codes returns all ICD10 ancestor codes when default parameters",{
  expect_parent_codes(dict,"J45.9",c("J45"))
  expect_parent_codes(dict,"J44.0",c("J44"))
  expect_parent_codes(dict,"C02.4",c("C02"))
})

test_that("get_parent_codes returns same ICD10 child codes when immediate_parents",{
  expect_parent_codes(dict,"J45.9",c("J45"),immediate=T)
  expect_parent_codes(dict,"J44.0",c("J44"),immediate=T)
  expect_parent_codes(dict,"C02.4",c("C02"),immediate=T)
})

cc_disconnect(dict)

