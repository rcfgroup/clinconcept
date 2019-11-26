skip_if_not(is_sqlite_available())
browser()
dict<-setup_test_dict("NHSICD10",T)

context("ICD10 child code retrieval functions")

#' importFrom clinconcept "expect_child_codes"
test_that("get_child_codes returns all ICD10 descendent codes when default parameters",{
  skip_on_cran()
  expect_child_codes(dict,"J44",c("J44.0","J44.1","J44.8","J44.9"))
  expect_child_codes(dict,"J45",c("J45.0","J45.1","J45.8","J45.9"));
  expect_child_codes(dict,"C02",c("C02.0","C02.1","C02.2","C02.3","C02.4","C02.8","C02.9"));
})

test_that("get_child_codes returns same ICD10 descendent codes when immediate_children flag",{
  skip_on_cran()
  expect_child_codes(dict,"J44",c("J44.0","J44.1","J44.8","J44.9"),immediate=T)
  expect_child_codes(dict,"J45",c("J45.0","J45.1","J45.8","J45.9"),immediate=T);
  expect_child_codes(dict,"C02",c("C02.0","C02.1","C02.2","C02.3","C02.4","C02.8","C02.9"),immediate=T);
})

cc_disconnect(dict)


