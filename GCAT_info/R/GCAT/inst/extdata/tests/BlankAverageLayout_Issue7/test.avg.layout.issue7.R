#  Test Issue7 Fix
#  GCAT normalize.ODs option to subtract an average of control wells at each time point for single plate

#  Initializing Variables

context("Blank Average Layout Testing")

INPUT.FILE = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10.csv",package="GCAT")
INPUT.LAYOUT = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10_Layout.csv",package="GCAT")
start.index = 1
normalize.method = "average.layout"
blank.value = NULL

add.constant = 0

library(GCAT)

#  Load data and initializing well.array to test our normalize.ODs
load(system.file("extdata/tests/BlankAverageLayout_Issue7", "wellArray.RData", package="GCAT"))
well.array = GCAT:::normalize.ODs(well.array, normalize.method = normalize.method, 
                               start.index = start.index, blank.value = blank.value, add.constant = add.constant)

#  Test output well.array for any issues
test_that("output well array is returned", {
  expect_equal(typeof(well.array), "list")
  expect_is(well.array, "array")
  expect_equal(length(well.array), 96)
  expect_true(all(sapply(well.array, is, "well")))
})

#  Test output well A1 norm field is a numeric list of length 135
test_that("output well array has norm as a list", {
  expect_is(well.array[[1,1,1]]@norm, "numeric")
  expect_equal(length(well.array[[1,1,1]]@norm), 136)
})

test_that("output average of 38th timestamp of well A1", {
  expect_equal(well.array[[1,1,1]]@norm[38], 0.08868409)
})



