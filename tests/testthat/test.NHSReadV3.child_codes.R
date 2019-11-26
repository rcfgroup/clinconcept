skip_if_not(is_sqlite_available(),"SQLite must be installed to run these tests")
exp_h3_readcodes<-c("H3122", "H312z", "H3y..", "H3y0.", "H3z..", "H4641", "Hyu31",
                    "X101i", "X101l", "X101m", "X102z", "Xa35l", "XaEIV", "XaEIW",
                    "XaEIY", "XaIND", "XaN4a", "XaZd1")

dict<-setup_test_dict("NHSReadV3",T)

context("READ V3 child code retrieval functions")

test_that("get_child_codes returns all READ v3 descendent codes when default parameters",{
  expect_child_codes(dict,"H3...",exp_h3_readcodes)
  expect_child_codes(dict,"H31..",c("H310.","XE0YM","X101j","H31y1","H3122","H3121","H312z","H31y.","H31z.","XE0ZN","H310z","XaZd1","Xa35l","H3y0.","X101i","H31yz","X101k","H311.","H311z","H313."))
  expect_child_codes(dict,"H32..",c("X101n","H3200","H3201","H3202","H320z","H32y2","H321.","H322.","H32y.","H32z.","H32y0","Hyu30","XE0YO","XE0YP","XE0YN","H32yz","X101o","X101p","H320.","H582.","X101q","X101r","XaIQg","H4640"))
})

test_that("get_child_codes returns filtered READ v3 descendent codes when immediate_children flag",{
  expect_child_codes(dict,"H3...",c("H3122", "H312z", "H3y..", "H3z..", "Hyu31", "X101l", "XaEIV",
                                    "XaEIW", "XaEIY", "XaIND", "XaN4a"),immediate=T)
  expect_child_codes(dict,"H31..",c("H310.", "H3121", "H3122", "H312z", "H31y.", "H31y1", "H31z.",
                                    "X101j", "XE0YM", "XE0ZN"),immediate=T)
  expect_child_codes(dict,"H32..",c("H321.", "H322.", "H32y.", "H32y2", "H32z.", "X101n"),immediate=T)
})

test_that("get_child_codes returns filtered READ v3 descendent codes when current_only flag",{
  expect_child_codes(dict,"H3...",c("H3122", "H4641", "X101l", "X101m", "X102z", "Xa35l", "XaIND",
                                    "XaZd1"),current=T)
  expect_child_codes(dict,"H31..",c("H310.","H3122","H31y1","X101j","X101k","Xa35l","XaZd1","XE0YM"),current=T)
  expect_child_codes(dict,"H32..",c("H320.", "H321.", "H322.", "H32y2", "H582.", "X101n", "X101o",
                                   "X101p", "X101q", "X101r", "XaIQg"),current=T)
})

test_that("get_child_codes returns filtered READ v3 descendent codes when immediate_children and current_only flag",{
  expect_child_codes(dict,"H3...",c("H3122", "X101l", "XaIND"),immediate=T,current=T)
  expect_child_codes(dict,"H31..",c("H310.", "H3122", "H31y1", "X101j", "XE0YM"),immediate=T,current=T)
  expect_child_codes(dict,"H32..",c("H321.", "H322.", "H32y2", "X101n"),immediate=T,current=T)
})

cc_disconnect(dict)


