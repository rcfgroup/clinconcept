skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
dict<-setup_test_dict("NHSReadV2",F)
context("READ V2 search concepts function")

test_that("search_concepts using read_code filters returns correct rows through dplyr",{
  enable_case_sensitivity(dict)

  obs_terms<-dplyr::collect(search_concepts(dict,read_code=="H3..."))
  expect_equal(nrow(obs_terms),1)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive pulmonary disease")

  obs_terms<-dplyr::collect(search_concepts(dict,(read_code=="H3..." | read_code=="H31..")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive pulmonary disease")
  expect_equal(obs_terms[2,]$term,"Chronic bronchitis")

  obs_terms<-dplyr::collect(search_concepts(dict,read_code %in% c("H3...","H32..")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive pulmonary disease")
  expect_equal(obs_terms[2,]$term,"Emphysema")

  obs_terms<-dplyr::collect(search_concepts(dict,read_code %like% "H3%" & (!read_code=="H311.")))
  expect_equal(nrow(obs_terms),100)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive pulmonary disease")
  expect_equal(obs_terms[22,]$term,"Other chronic bronchitis")
  disable_case_sensitivity(dict)
})

test_that("search_concepts using terms returns correct rows through dplyr",{
  disable_case_sensitivity(dict)
  obs_terms<-dplyr::collect(search_concepts(dict,term=="Chronic obstructive airways disease NOS"))
  expect_equal(nrow(obs_terms),1)
  expect_equal(obs_terms[1,]$read_code,"H3z..")

  obs_terms<-dplyr::collect(search_concepts(dict,term=="Chronic obstructive airways disease NOS" | term=="Chronic bronchitis"))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$read_code,"H31..")
  expect_equal(obs_terms[2,]$read_code,"H3z..")

  obs_terms<-dplyr::collect(search_concepts(dict,term %in% c("Chronic obstructive airways disease NOS","Emphysema")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$read_code,"H32..")
  expect_equal(obs_terms[2,]$read_code,"H3z..")

  obs_terms<-dplyr::collect(search_concepts(dict,term %like% "%chronic bronchitis%" & (!term %like% "purulent")))
  expect_equal(nrow(obs_terms),13)
  expect_equal(obs_terms[1,]$read_code,"H31..")
  expect_equal(obs_terms[1,]$term,"Chronic bronchitis")
  expect_equal(obs_terms[9,]$read_code,"H312z")
  expect_equal(obs_terms[9,]$term,"Obstructive chronic bronchitis NOS")
})

test_that("search_concepts returns different vector outputs",{
  disable_case_sensitivity(dict)
  obs_codes<-search_concepts(dict,term=="Chronic obstructive pulmonary disease",output="codes")
  expect_equal(obs_codes,c("H3..."))

  obs_codes<-search_concepts(dict,term=="Chronic obstructive pulmonary disease" | term=="Chronic bronchitis",output="codes")
  expect_equal(obs_codes,c("H3...","H31.."))

  enable_case_sensitivity(dict)

  obs_terms<-search_concepts(dict,read_code=="H3...",output="terms")
  expect_equal(obs_terms,c("Chronic obstructive pulmonary disease"))

  obs_terms<-search_concepts(dict,(read_code=="H3..." | read_code=="H31.."),output="terms")
  expect_equal(obs_terms,c("Chronic obstructive pulmonary disease","Chronic bronchitis"))
})

cleanup_test_dict(dict)


