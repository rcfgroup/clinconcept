library(clinconcept)
library(testthat)
library(readr)
if(!is_sqlite_available()) {
  return(NA)
}

dict<-setup_test_dict("NHSSnomedCT",T)

context("SNOMED-CT child code retrieval functions")

test_that("get_child_codes returns all NHS SNOMED-CT descendent codes when default parameters",{
  skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
  skip_on_cran()
  expect_child_codes(dict,"389145006",c("1945002", "233682008", "30352005", "59786004", "233683003",
                                        "389146007", "424643009", "423889005", "10675871000119106", "10676391000119108",
                                        "10675431000119106", "703954005", "703953004", "195972005", "233681001",
                                        "389145006", "41553006", "56968009", "57607007", "63088003",
                                        "67415000", "91340006", "404805002", "92807009", "708095007",
                                        "708093000", "12428000", "59327009", "233685005", "195976008",
                                        "708094006", "708096008", "10675911000119109", "10676431000119103",
                                        "10675471000119109"))
})

test_that("get_child_codes returns filtered SNOMED-CT descendent codes when immediate_children flag",{
  expect_child_codes(dict,"389145006",c("1945002", "233682008", "30352005", "59786004", "233683003",
                                    "389146007", "424643009", "423889005", "10675871000119106", "10676391000119108",
                                    "10675431000119106"),immediate=T)
  expect_child_codes(dict,"30352005",c("703954005","703953004"),immediate=T)
})

test_that("get_child_codes returns filtered SNOMED-CT descendent codes when current_only flag",{
  skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
  skip_on_cran()
  expect_child_codes(dict,"30352005",c("703954005","703953004"),current=T)

})

cc_disconnect(dict)


