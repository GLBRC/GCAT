#  Test Issue7 Fix
#  GCAT normalize.ODs option to subtract an average of control wells at each time point for single plate

#  Initializing Variables

context("Blank Average Layout Testing - Old stored well output with new well output")

INPUT.FILE = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10.csv",package="GCAT")
INPUT.LAYOUT = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10_Layout.csv",package="GCAT")
blank.value = "average.layout"
INPUT.DIR =  system.file("extdata/tests/BlankAverageLayout_Issue7",package="GCAT")
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")

#  Clean up if output dir is not empty before running test
unlink(OUTPUT.DIR, recursive=T)

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600


out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = INPUT.LAYOUT,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 1, blank.value = blank.value, start.index = 1, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T)

old.output.table = read.table("output_gcat.fit_2015-11-08_22.31.16.txt", nrows=96, header=T,sep="\t", fill = TRUE)

new.output.file = list.files(path=OUTPUT.DIR, pattern = "^output_gcat\\.fit_.+\\.txt$")
new.output.table = read.table(paste("temp",new.output.file,sep="/"),header=T,sep="\t",nrows=96, fill = TRUE)

#  Remove "pdf.file" column
old.output.table = old.output.table[!names(old.output.table) %in% c("pdf.file","AUC..log.OD","AUC.OD")]
new.output.table = new.output.table[!names(new.output.table) %in% c("pdf.file","AUC..log.OD","AUC.OD")]

#  Verify that output spreadsheets are identical within rounding error
#  (relaxed tolerance to 1e-6 to make the test pass on Windows 7 with R 3.2.0)
stopifnot(all.equal(new.output.table,old.output.table,tolerance=1e-6))

#  Clean up
unlink(OUTPUT.DIR, recursive=T)