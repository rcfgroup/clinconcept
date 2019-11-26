skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
dict<-setup_test_dict("NHSReadV3",F)
context("READ V3 search concepts function")

test_that("search_concepts using read_code filters returns correct rows through dplyr",{
  enable_case_sensitivity(dict)

  obs_terms<-dplyr::collect(search_concepts(dict,read_code=="H3..." & synonym!='1'))
  expect_equal(nrow(obs_terms),1)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive lung disease")

  obs_terms<-dplyr::collect(search_concepts(dict,(read_code=="H3..." | read_code=="H31..") & synonym!='1'))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive lung disease")
  expect_equal(obs_terms[2,]$term,"Chronic bronchitis")

  obs_terms<-dplyr::collect(search_concepts(dict,read_code %in% c("H3...","H32..") & synonym!='1'))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive lung disease")
  expect_equal(obs_terms[2,]$term,"Emphysema")

  obs_terms<-dplyr::collect(search_concepts(dict,read_code %like% "H3%" & (!read_code=="H311.") & synonym!='1'))
  expect_equal(nrow(obs_terms),22)
  expect_equal(obs_terms[1,]$term,"Chronic obstructive lung disease")
  expect_equal(obs_terms[22,]$term,"Chronic obstructive airways disease NOS")

  disable_case_sensitivity(dict)
})

test_that("search_concepts using terms returns correct rows through dplyr",{

  disable_case_sensitivity(dict)
  obs_terms<-collect(search_concepts(dict,term=="Chronic obstructive lung disease"))
  expect_equal(nrow(obs_terms),1)
  expect_equal(obs_terms[1,]$read_code,"H3...")

  obs_terms<-collect(search_concepts(dict,term=="Chronic obstructive lung disease" | term=="Chronic bronchitis"))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$read_code,"H3...")
  expect_equal(obs_terms[2,]$read_code,"H31..")

  obs_terms<-collect(search_concepts(dict,term %in% c("Chronic obstructive lung disease","Emphysema")))
  expect_equal(nrow(obs_terms),2)
  expect_equal(obs_terms[1,]$read_code,"H3...")
  expect_equal(obs_terms[2,]$read_code,"H32..")

  obs_terms<-collect(arrange(search_concepts(dict,term %like% "%chronic bronchitis%" & (!term=="Chronic bronchitis")),read_code))

  expect_equal(nrow(obs_terms),13)
  expect_equal(obs_terms[1,]$read_code,"H3...")
  expect_equal(obs_terms[1,]$term,"Obstructive chronic bronchitis")
  expect_equal(obs_terms[13,]$read_code,"XE0ZN")
  expect_equal(obs_terms[13,]$term,"Chronic bronchitis NOS")
})

test_that("search_concepts returns different vector outputs",{
  disable_case_sensitivity(dict)
  obs_codes<-search_concepts(dict,term=="Chronic obstructive lung disease",output="codes")
  expect_equal(obs_codes,c("H3..."))

  obs_codes<-search_concepts(dict,term=="Chronic obstructive lung disease" | term=="Chronic bronchitis",output="codes")
  expect_equal(obs_codes,c("H3...","H31.."))

  enable_case_sensitivity(dict)

  obs_terms<-search_concepts(dict,read_code=="H3..." & synonym!='1',output="terms")
  expect_equal(obs_terms,c("Chronic obstructive lung disease"))

  obs_terms<-search_concepts(dict,(read_code=="H3..." | read_code=="H31..") & synonym!='1',output="terms")
  expect_equal(obs_terms,c("Chronic obstructive lung disease","Chronic bronchitis"))
})

test_that("search_concepts with include_synonyms returns correct data",{
  enable_case_sensitivity(dict)

  obs_rows<-collect(search_concepts(dict,read_code=="H3...",include_synonyms=T))
  expect_equal(nrow(obs_rows),17)
  expect_equal(obs_rows[1,]$term,"Chronic obstructive lung disease")

  obs_rows<-collect(search_concepts(dict,term %like% "%airway%",include_synonyms=T))
  expect_equal(nrow(obs_rows),10)

  obs_codes<-search_concepts(dict,term %like% "%airway%",output="codes",include_synonyms=T)
  expect_equal(length(obs_codes),6)
  disable_case_sensitivity(dict)

  obs_terms<-search_concepts(dict,read_code=="H3...",include_synonyms=T,output="terms")
  expect_equal(length(obs_terms),17)
  expect_equal(obs_terms[1],"Chronic obstructive lung disease")
})

cleanup_test_dict(dict)


