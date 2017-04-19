#  Test that extra columns other than Time and OD are being ignored

INPUT.DIR1 =  system.file("extdata/tests/YPDAFEXglucoseTests_2-25-10",package="GCAT")
OUTPUT.DIR.1 = paste(system.file("extdata/tests/Issue9.IgnoreUnneededColumns",package="GCAT"),"temp1",sep="/")
INPUT.FILE1 = "YPDAFEXglucoseTests_2-25-10.csv"

INPUT.DIR2 =  system.file("extdata/tests/Issue9.IgnoreUnneededColumns",package="GCAT")
OUTPUT.DIR.2 = paste(system.file("extdata/tests/Issue9.IgnoreUnneededColumns",package="GCAT"),"temp2",sep="/")
INPUT.FILE2 = "ExtraColumns.csv"

unlink(OUTPUT.DIR.1, recursive=T)
unlink(OUTPUT.DIR.2, recursive=T)

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR1)
time.input=1/3600

# - Read normal file
out1 = gcat.analysis.main(file.list = INPUT.FILE1, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR.1, graphic.dir = OUTPUT.DIR.1, 
                         add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T)

setwd(INPUT.DIR2)
# - Read extra column csv file
out2 = gcat.analysis.main(file.list = INPUT.FILE2, single.plate = T, layout.file = NULL,   
                          out.dir = OUTPUT.DIR.2, graphic.dir = OUTPUT.DIR.2, 
                          add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                          use.linear.param=F, use.loess=F, smooth.param=0.1,
                          points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                          silent = F, verbose = T, return.fit = T, overview.jpgs = T)

#  Verify that specifying parameter ranges did not affect any computations except for how the heat maps are drawn
all.equal(out1,out2)

#  Clean up
unlink(OUTPUT.DIR.1, recursive=T)
unlink(OUTPUT.DIR.2, recursive=T)