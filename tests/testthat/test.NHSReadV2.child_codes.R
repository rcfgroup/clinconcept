skip_if_not(is_sqlite_available())
exp_h2_readcodes<-c("H30..", "H300.", "H301.", "H302.", "H30z.", "H31..", "H310.",
                    "H3100", "H3101", "H310z", "H311.", "H3110", "H3111", "H311z",
                    "H312.", "H3120", "H3121", "H3122", "H3123", "H312z", "H313.",
                    "H31y.", "H31y0", "H31y1", "H31yz", "H31z.", "H32..", "H320.",
                    "H3200", "H3201", "H3202", "H3203", "H320z", "H321.", "H322.",
                    "H32y.", "H32y0", "H32y1", "H32y2", "H32yz", "H32z.", "H33..",
                    "H330.", "H3300", "H3301", "H330z", "H331.", "H3310", "H3311",
                    "H331z", "H332.", "H333.", "H334.", "H335.", "H33z.", "H33z0",
                    "H33z1", "H33z2", "H33zz", "H34..", "H340.", "H341.", "H34z.",
                    "H35..", "H350.", "H351.", "H352.", "H3520", "H3521", "H352z",
                    "H353.", "H354.", "H355.", "H356.", "H357.", "H35y.", "H35y0",
                    "H35y1", "H35y2", "H35y3", "H35y4", "H35y5", "H35y6", "H35y7",
                    "H35y8", "H35yz", "H35z.", "H35z0", "H35z1", "H35zz", "H36..",
                    "H37..", "H38..", "H39..", "H3A..", "H3B..", "H3y..", "H3y0.",
                    "H3y1.", "H3z..");

dict<-setup_test_dict("NHSReadV2",T)

context("READ V2 child code retrieval functions")

test_that("get_child_codes returns all READ v2 descendent codes when default parameters",{
  expect_child_codes(dict,"H3...",exp_h2_readcodes)
  expect_child_codes(dict,"H31..",c("H310.", "H3100", "H3101", "H310z", "H311.", "H3110", "H3111",
                                    "H311z", "H312.", "H3120", "H3121", "H3122", "H3123", "H312z",
                                    "H313.", "H31y.", "H31y0", "H31y1", "H31yz", "H31z."));
  expect_child_codes(dict,"H32..",c("H320.", "H3200", "H3201", "H3202", "H3203", "H320z", "H321.",
                                    "H322.", "H32y.", "H32y0", "H32y1", "H32y2", "H32yz", "H32z."));
})

test_that("get_child_codes returns filtered READ V2 descendent codes when immediate_children flag",{
  expect_child_codes(dict,"H3...",c("H30..", "H31..", "H32..", "H33..", "H34..", "H35..", "H36..", "H37..", "H38..", "H39..", "H3A..", "H3B..", "H3y..", "H3z.."),immediate=T)
  expect_child_codes(dict,"H31..",c("H310.", "H311.", "H312.","H313.", "H31y.", "H31z."),immediate=T);
  expect_child_codes(dict,"H32..",c("H320.", "H321.", "H322.", "H32y.", "H32z."),immediate=T)
})

cc_disconnect(dict)
#remove_test_dict(dict)

