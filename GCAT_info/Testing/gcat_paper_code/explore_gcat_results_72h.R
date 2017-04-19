#  Explore GCAT results file for the 72h time course with 2 strains

WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/GCAT 4/2_Piotrowski_data_analysis/working_data/GCAT_4.3_results/72h 128 vs 3d/loess_0.01"
GCAT.FILE = "output_gcat.fit_2014-09-24_17.16.31.txt"

#### Load data ####
setwd(WORK.DIR)
data1  = read.delim(GCAT.FILE,nrows=96)

#### Assign strain id ####
strain = rep("Lab",96)
strain[grep("[ABCD]",data1$well)] = "Y128"

#### Summarize fits of the lab strain ####
summary(data1[strain=="Lab",])

#  Why is reach so low?
summary(data1$reach[strain=="Lab"])

#### Compute CV of key parameters ####
cv = function(x) {sd(x,na.rm=T)/mean(x,na.rm=T)}

if ("shape.par" %in% names(data1)) {
  apply(data1[strain=="Lab",c("lag.time","spec.growth","tot.growth","shape.par")],2,cv)
} else {
  apply(data1[strain=="Lab",c("lag.time","spec.growth","tot.growth")],2,cv)  
}
