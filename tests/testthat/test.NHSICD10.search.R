skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")

dict<-setup_test_dict("NHSICD10",F)
context("ICD10 search concepts function")

test_that("search_concepts using icd10_code filters returns correct rows through dplyr",{

  enable_case_sensitivity(dict)

  obs_terms<-dplyr::collect(search_concepts(dict,icd10_code=="J45.0"))
  expect_equal(nrow(obs_terms),1)
  expect_equal(obs_terms[1,]$term,"Predominantly allergic asthma")

  obs_terms<-dplyr::collect(search_concepts(dict,(icd10_code=="J45.0" | icd10_code=="J44.0")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive pulmonary disease with acute lower respiratory infection")
  expect_equal(obs_terms[2,]$term,"Predominantly allergic asthma")

  obs_terms<-dplyr::collect(search_concepts(dict,icd10_code %in% c("J45.1","J45.0")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Predominantly allergic asthma")
  expect_equal(obs_terms[2,]$term,"Nonallergic asthma")

  obs_terms<-dplyr::collect(search_concepts(dict,icd10_code %like% "J45%" & (!icd10_code=="J45.9")))
  expect_equal(nrow(obs_terms),4)
  expect_equal(obs_terms[1,]$term,"Asthma")
  expect_equal(obs_terms[4,]$term,"Mixed asthma")
  disable_case_sensitivity(dict)
})

cleanup_test_dict(dict)


