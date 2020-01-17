skip_if_not(is_sqlite_available())
dict<-setup_test_dict("NHSSnomedCT",T)

context("SNOMED-CT parent code retrieval functions")

test_that("get_parent_codes returns all NHS SNOMED-CT ascendent codes when default parameters",{
  expect_parent_codes(dict,"703954005",c("12263007", "55985003", "30352005", "187214007", "112476003",
                                         "82094008", "89187006", "441862004", "472964009", "424643009",
                                         "92807009", "711092006", "16862005", "39607008", "116003000",
                                         "955009", "26036001", "389145006", "106182000", "233795005",
                                         "419076005", "36272005", "79688008", "427286007", "50417007",
                                         "105590001", "127072000", "389146007", "195967001", "418168000",
                                         "421871004", "421092003", "418925002", "20139000", "422076005",
                                         "424199006"));
  expect_parent_codes(dict,"389146007",c("26036001", "195967001", "39607008", "955009", "127072000",
                                         "116003000", "106182000", "389145006", "419076005", "12263007",
                                         "55985003", "389146007", "418168000", "421871004", "421092003",
                                         "418925002", "20139000", "472964009", "89187006"))
})

test_that("get_parent_codes returns filtered SNOMED-CT ascendent codes when immediate_parent flag",{
  expect_parent_codes(dict,"703954005",c("12263007", "55985003", "30352005", "187214007", "112476003",
                                        "82094008", "89187006", "441862004", "472964009", "424643009",
                                        "92807009", "711092006"),immediate=T)
  expect_parent_codes(dict,"389146007",c("26036001", "195967001", "39607008", "955009", "127072000",
                                         "116003000", "106182000", "389145006", "419076005", "12263007",
                                         "55985003"),immediate=T)
})

test_that("get_child_codes returns filtered SNOMED-CT descendent codes when current_only flag",{
  expect_parent_codes(dict,"703954005",c("30352005", "389145006", "389146007", "424643009", "92807009"
                                         ),current=T)

})

testthat::teardown(cleanup_test_dict(dict))


