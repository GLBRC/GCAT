context("Return errors must be a string (or as.character) for Rails to display them properly")

INPUT.DIR =  system.file("extdata/default_examples",package="GCAT")
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")
INPUT.FILE = "single_YPDAFEXglucoseTests_2-25-10.csv"

#  Clean up if output dir is not empty before running test
unlink(OUTPUT.DIR, recursive=T)

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600

#  Test for inoculation timepoint is 1 and blank value is NULL
expect_equal(gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                               out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                               add.constant = 1, blank.value = NULL, start.index = 1, growth.cutoff = 0.05,
                               use.linear.param=F, use.loess=F, smooth.param=0.1,
                               points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                               silent = F, verbose = T, return.fit = T, overview.jpgs = T), "If inoculation time point is 1, the user must specify a blank value")

#  Clean up
unlink(OUTPUT.DIR, recursive=T)