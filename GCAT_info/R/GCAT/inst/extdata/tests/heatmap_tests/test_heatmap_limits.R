#  Test that heatmaps are processed and generated correctly
#  Yury V Bukhman, 01 July 2015
#  This script generates heat maps with pre-set limits.  Manually check output directory to see if they make sense.

INPUT.DIR =  system.file("extdata/tests/YPDAFEXglucoseTests_2-25-10",package="GCAT")
OUTPUT.DIR.1 = paste(system.file("extdata/tests/heatmap_tests",package="GCAT"),"temp1",sep="/")
OUTPUT.DIR.2 = paste(system.file("extdata/tests/heatmap_tests",package="GCAT"),"temp2",sep="/")
INPUT.FILE = "YPDAFEXglucoseTests_2-25-10.csv"

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600

# - do specify parameter ranges
out2 = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR.2, graphic.dir = OUTPUT.DIR.2, 
                         add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T,
                         lagRange = c(0,3), totalRange = c(0.2,0.82), totalODRange = c(0.06,1), specRange = c(0.04,0.13))

# - do not specify parameter ranges
out1 = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                          out.dir = OUTPUT.DIR.1, graphic.dir = OUTPUT.DIR.1, 
                          add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                          use.linear.param=F, use.loess=F, smooth.param=0.1,
                          points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                          silent = F, verbose = T, return.fit = T, overview.jpgs = T)

#  Verify that specifying parameter ranges did not affect any computations except for how the heat maps are drawn
all.equal(out1,out2)