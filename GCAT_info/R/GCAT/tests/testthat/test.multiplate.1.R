#  Test GCAT processing of an example multiplate file with default settings as of revision 537

INPUT.FILE = system.file("extdata/default_examples","multi_test_YYYY_MM_DD_HH_MM_SS.csv",package="GCAT")
OUTPUT.DIR = paste(getwd(),"GCAT_test_out_multiplate.1",sep="/")

library(GCAT)
context("Multiplate example with parameter set 1")

#  Run GCAT
out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = F, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 0.1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input="%Y-%m-%d %H:%M:%S",
                         silent = T, verbose = F, return.fit = T, overview.jpgs = T)

# Test output well array
test_that("output well array is returned", {
  expect_equal(typeof(out), "list")
  expect_is(out, "array")
  expect_equal(length(out), 192)
  expect_true(all(sapply(out, is, "well")))
})

#  Test well D05 from plate 2
test_that("well D05 from plate 2 is fit", {
  expect_is(out[[132]],"well")
  expect_true(getPosition(out[[132]])["row"]=="D")
  expect_true(getPosition(out[[132]])["col"]=="5")
  expect_false(getCurPar(out[[132]])$no.growth)
  expect_is(getFitPar(out[[132]])$u,"numeric")
})

#  Clean up
unlink(OUTPUT.DIR, recursive=T)