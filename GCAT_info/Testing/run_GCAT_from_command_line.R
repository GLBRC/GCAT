# Run GCAT from command line

INPUT.DIR =  "C:/Users/ybukhman/Desktop/debug_info"
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")
INPUT.FILE = "Take this one.csv"

#  Clean up if output dir is not empty before running test
unlink(OUTPUT.DIR, recursive=T)

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600
out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 0, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T)


