context("HeatMap test")

INPUT.DIR =  system.file("extdata/default_examples",package="GCAT")
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")
OUTPUT.DIR.1 = paste(INPUT.DIR, "temp1", sep="/")
INPUT.FILE = "single_YPDAFEXglucoseTests_2-25-10.csv"

#  Clean up if output dir is not empty before running test
unlink(OUTPUT.DIR, recursive=T)

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600


#  Test for (Inf, NA) in HeatMap range should be finite
expect_equal(gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                                out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                                add.constant = 1, blank.value = 0.4, start.index = 1, growth.cutoff = 0.05,
                                use.linear.param=F, use.loess=F, smooth.param=0.1,
                                points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                                silent = F, verbose = T, return.fit = T, overview.jpgs = T, lagRange = c(Inf, NA), specRange = c(0.154, 0.159), totalRange = c(NA, 1.039), totalODRange = c(0.063, 1.40)), "Heat map range must contain either NA or a finite number.  Bad range: Inf-NA")


#  Test for (-2, NA) HeatMap range should be non negative
expect_equal(gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                               out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                               add.constant = 1, blank.value = 0.4, start.index = 1, growth.cutoff = 0.05,
                               use.linear.param=F, use.loess=F, smooth.param=0.1,
                               points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                               silent = F, verbose = T, return.fit = T, overview.jpgs = T, lagRange = c(-2, NA), specRange = c(0.154, 0.159), totalRange = c(NA, 1.039), totalODRange = c(NA, NA)), "Heat map range must be positive.  Bad range: -2-NA")



#  Test for (NA, NA)
# - do specify parameter ranges
out2 = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                          out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                          add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                          use.linear.param=F, use.loess=F, smooth.param=0.1,
                          points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                          silent = F, verbose = T, return.fit = T, overview.jpgs = T,
                          lagRange = c(NA, NA), totalRange = c(0.2,0.82), totalODRange = c(0.06,1), specRange = c(0.04,0.13))
# - do not specify parameter ranges
out1 = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                          out.dir = OUTPUT.DIR.1, graphic.dir = OUTPUT.DIR.1, 
                          add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                          use.linear.param=F, use.loess=F, smooth.param=0.1,
                          points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                          silent = F, verbose = T, return.fit = T, overview.jpgs = T)
#  Verify that specifying parameter ranges did not affect any computations except for how the heat maps are drawn
all.equal(out1,out2)

#  Clean up
unlink(OUTPUT.DIR, recursive=T)
unlink(OUTPUT.DIR.1, recursive=T)