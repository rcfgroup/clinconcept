setup_test_dict<-function(dict_type,force_create=F) {
  test_path = getwd()
  db_path<-paste0(test_path,"/test_",dict_type,".sqlite")

  if(force_create && file.exists(db_path)) {
    unlink(db_path)
  }
  dict<-rcc_from_list(dict_type,list(type="sqlite",dbname=db_path))
  dict$sql_path=paste0(test_path,"/../../sql")
  build_concept_tables(dict,paste0(test_path,paste0("/data/",dict_type)))
  dict
}
expect_parent_codes<-function(dict,code,exp_parent_codes, immediate=F, current=F) {
  obs_parent_codes<-get_parent_codes(dict,code,output_codes=T,immediate_descendents=immediate,current_only=current)
  expect_equal(obs_parent_codes,exp_parent_codes)
}
exp_h3_readcodes<-c(".....","H....","X0003","XaBVJ")

dict<-setup_test_dict("NHSReadV3",F)

context("parent code retrieval functions")

test_that("get_parent_codes returns all READ v3 descendent rows when default parameters",{
  obs_parentren<-get_parent_codes(dict,"H3...") %>% collect()
  expect_equal(obs_parentren$read_code,exp_h3_readcodes)
  expect_equal(nrow(obs_parentren),3)
  obs_row1<-as.list(obs_parentren[1,])
  exp_row1<-list(read_code = ".....", term_30 = "Read thesaurus",
                 term_60 = NA, term_198 = NA, term_id="Y106U",term="Read thesaurus",synonym = "0",
                 desc_type="P",status = "C")
  expect_equal(obs_row1,exp_row1)

  row3<-as.list(obs_parentren[3,])
  expect_equal(list(read_code = "X0003", term_30 = "Disorders", term_60 = NA,
         term_198 = NA, term_id="Y006t", term="Disorders", synonym = "0", desc_type="P", status = "C"),row3)
})
# test_that("get_parent_codes returns all READ v3 descendent codes when default parameters",{
#   expect_parent_codes(dict,"H3...",exp_h3_readcodes)
#   expect_parent_codes(dict,"H31..",c("H310.", "H310z", "H311.", "H311z", "H3121", "H3122", "H312z",
#                                     "H313.", "H31y.", "H31y1", "H31z.", "X101j", "X101k", "XE0YM",
#                                     "XE0ZN"))
#   expect_parent_codes(dict,"H32..",c("H320.", "H321.", "H322.", "H32y.", "H32y2", "H32z.", "H582.",
#                                     "X101n", "X101o", "X101p", "X101q", "X101r", "XaIQg"))
# })
#
# test_that("get_parent_codes returns filtered READ v3 descendent codes when immediate_descendents flag",{
#   expect_parent_codes(dict,"H3...",c("H3122", "H312z", "H3y..", "H3z..", "Hyu31", "X101l", "XaEIV",
#                                     "XaEIW", "XaEIY", "XaIND", "XaN4a"),immediate=T)
#   expect_parent_codes(dict,"H31..",c("H310.", "H3121", "H3122", "H312z", "H31y.", "H31y1", "H31z.",
#                                     "X101j", "XE0YM", "XE0ZN"),immediate=T)
#   expect_parent_codes(dict,"H32..",c("H321.", "H322.", "H32y.", "H32y2", "H32z.", "X101n"),immediate=T)
# })
#
# test_that("get_parent_codes returns filtered READ v3 descendent codes when current_only flag",{
#   expect_parent_codes(dict,"H3...",c("H3122", "H4641", "X101l", "X101m", "X102z", "Xa35l", "XaIND",
#                                     "XaZd1"),current=T)
#   expect_parent_codes(dict,"H31..",c("H310.", "H3122", "H31y1", "X101j", "X101k", "XE0YM"),current=T)
#   expect_parent_codes(dict,"H32..",c("H320.", "H321.", "H322.", "H32y2", "H582.", "X101n", "X101o",
#                                     "X101p", "X101q", "X101r", "XaIQg"),current=T)
# })
#
# test_that("get_parent_codes returns filtered READ v3 descendent codes when immediate_descendents and current_only flag",{
#   expect_parent_codes(dict,"H3...",c("H3122", "X101l", "XaIND"),immediate=T,current=T)
#   expect_parent_codes(dict,"H31..",c("H310.", "H3122", "H31y1", "X101j", "XE0YM"),immediate=T,current=T)
#   expect_parent_codes(dict,"H32..",c("H321.", "H322.", "H32y2", "X101n"),immediate=T,current=T)
# })
#
#
