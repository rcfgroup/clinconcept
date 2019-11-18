library(clinconcept)
library(testthat)
library(readr)
dict<-setup_test_dict("NHSSnomedCT",F)
context("SNOMED-CT search concepts function")
test_that("search_concepts using snomed_code filters returns correct rows through dplyr",{
  skip_if_not(cc_is_available(dict),paste(dict$type, "must be installed to run these tests"))
  skip_on_cran()

  enable_case_sensitivity(dict)

  obs_terms<-search_concepts(dict,snomed_code=="1945002") %>% arrange(snomed_code) %>% collect()
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[2,]$term,"Allergic asthma with stated cause (disorder)")

  obs_terms<-search_concepts(dict,(snomed_code=="1945002" | snomed_code=="30352005")) %>% arrange(snomed_code) %>% collect()
  expect_equal(nrow(obs_terms),4)
  expect_equal(obs_terms[1,]$term,"Allergic asthma with stated cause")
  expect_equal(obs_terms[3,]$term,"Allergic-infective asthma")

  obs_terms<-search_concepts(dict,snomed_code %in% c("1945002","30352005")) %>% arrange(snomed_code) %>% collect()
  expect_equal(nrow(obs_terms),4)
  expect_equal(obs_terms[1,]$term,"Allergic asthma with stated cause")
  expect_equal(obs_terms[3,]$term,"Allergic-infective asthma")

  obs_terms<-search_concepts(dict,snomed_code %like% "19%" & (!snomed_code=="195972005")) %>% arrange(snomed_code) %>% collect()
  expect_equal(nrow(obs_terms),4)
  expect_equal(obs_terms[1,]$term,"Allergic asthma with stated cause")
  expect_equal(obs_terms[4,]$term,"Intrinsic asthma NOS (disorder)")

  disable_case_sensitivity(dict)
})

test_that("search_concepts using terms returns correct rows through dplyr",{
  skip_if_not(cc_is_available(dict),paste(dict$type, "must be installed to run these tests"))
  skip_on_cran()

  disable_case_sensitivity(dict)

  obs_terms<-collect(search_concepts(dict,term %like% "%asthma%" & term %like% "%allergic-infective%"))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$snomed_code,"30352005")
  expect_equal(obs_terms[2,]$snomed_code,"30352005")

  obs_terms<-collect(search_concepts(dict,term %in% c("Enzyme detergent lung","Wood asthma")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$snomed_code,"41553006")
  expect_equal(obs_terms[2,]$snomed_code,"56968009")

  obs_terms<-collect(arrange(search_concepts(dict,term %like% "%asthma%" & (!term %like% "%allergic%")),snomed_code))

  expect_equal(nrow(obs_terms),53)
  expect_equal(obs_terms[1,]$snomed_code,"12428000")
  expect_equal(obs_terms[1,]$term,"Intrinsic asthma without status asthmaticus")
  expect_equal(obs_terms[9,]$snomed_code,"233683003")
  expect_equal(obs_terms[9,]$term,"Hay fever with asthma")
})

test_that("search_concepts returns different vector outputs",{
  skip_if_not(cc_is_available(dict),paste(dict$type, "must be installed to run these tests"))
  skip_on_cran()

  disable_case_sensitivity(dict)
  obs_codes<-search_concepts(dict,term %like% "%allergic%" & term %like% "%asthma%",output="codes")

  expect_equal(obs_codes,c("1945002", "30352005", "233682008", "389145006", "424643009",
                           "423889005", "703953004", "703954005", "10675871000119106", "10675911000119109",
                           "10676391000119108", "10675431000119106", "10675471000119109",
                           "10676431000119103", "708093000", "708095007"))

  obs_codes<-search_concepts(dict,term == "Allergic asthma with stated cause" | term %like% "Allergic atopic asthma",output="codes")
  expect_equal(obs_codes,c("1945002","389145006"))

  enable_case_sensitivity(dict)

  obs_terms<-search_concepts(dict,(snomed_code=="1945002" | snomed_code=="389145006"),output="terms")
  expect_equal(obs_terms,c("Allergic asthma with stated cause", "Allergic asthma with stated cause (disorder)",
                           "Allergic asthma (disorder)", "Allergic asthma", "Atopic asthma",
                           "Allergic atopic asthma"))
})

cleanup_test_dict(dict)


