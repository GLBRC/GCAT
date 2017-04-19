#  Test AUC computation

library(GCAT)
load(system.file("extdata/tests/YPDAFEXglucoseTests_2-25-10","default_output.RData",package="GCAT"))

#  Compute upper bound of the AUC
well = out[[30]]
screen.data = getScreenData(well)
constant.added = screen.data$OD[1] - getNorm(well)
upper.bound = achieved.growth(well)*max(screen.data$Time)
upper.bound.OD = achieved.growth.OD(well,constant.added)*max(getScreenData(well)$Time)

#  Compute the AUC and check that it is in the right range
auc = auc(well)
auc.OD = auc.OD(well,constant.added)
test_that("auc is in the right range", {
  expect_less_than(auc, upper.bound)
  expect_less_than(auc.OD, upper.bound.OD)
  expect_more_than(auc, upper.bound/2)
  expect_more_than(auc.OD, upper.bound.OD/2)
})

#  Compute the AUC in a range and check that it's correct
auc = auc(well,start=10,end=15)
auc.approx = achieved.growth(well)*5
auc.OD = auc.OD(well,start=10,end=15,constant.added=constant.added)
auc.OD.approx = achieved.growth.OD(well,constant.added)*5
test_that("auc computed on an interval is correct", {
  expect_equal(auc,auc.approx,tolerance=0.1)
  expect_equal(auc.OD,auc.OD.approx,tolerance=0.1)
})
