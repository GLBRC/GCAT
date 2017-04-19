#  Compare 2 GCAT output files
#  Yury V Bukhman, 2013-09-04

#### Set up ####

#  Compare GCAT 3 to GCAT 2
#WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/B05_Yeast_HTS/B05.16 GCAT 3.2 testing Oct 2013/testing round 1"
#INPUT.1 = "GCAT 2 output/output_gcat.fit_2013-10-03_17.56.55.txt"
#INPUT.2 = "GCAT 3 output/output_gcat.fit_2013-10-03_18.08.26.txt"
#WORK.DIR = "C:/Users/Yury/Desktop/GCAT 4/3_GCAT_example_data_analysis"

#  Compare GCAT 4 to GCAT 3
#WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.19 GCAT 4/3_GCAT_example_data_analysis"
#INPUT.1 = "GCAT_3_V322/output_gcat.fit_2013-10-18_17.14.50.txt"
#NAME.1 = "GCAT_3_V322"
#INPUT.2 = "GCAT_4.6/singlePlateAnalysis_sigmoid/output_gcat.fit_2014-11-04_16.26.00.txt"
#NAME.2 = "GCAT_4.6"
#PARAMETERS = list(c("lag.time","lag.time..hrs","Lag Time"), c("spec.growth","max.spec.growth.rate..log.OD.hr","Max Specific Growth Rate"), c("tot.growth","projected.growth.OD","Projected Growth, OD"))

#  Compare sigmoid fits between different versions of GCAT 4
PARAMETERS = list(c("lag.time..hrs","lag.time..hrs","Lag Time"), c("inflection.time..hrs","inflection.time..hrs","Inflection Time"), c("max.spec.growth.rate..log.OD.hr","max.spec.growth.rate..log.OD.hr","Max Specific Growth Rate"), c("projected.growth.OD","projected.growth.OD","Projected Growth, OD"), c("achieved.growth.OD","achieved.growth.OD","Achieved Growth, OD"))
WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.19 GCAT 4/3_GCAT_example_data_analysis"
INPUT.1 = "/mnt/file.glbrc.org/shared/6.4.1 Bukhman/Public/GCAT4.5 Test/multiplePlateAnalysis_sigmoid_compare/output_gcat.fit_2014-11-06_13.35.43.txt"
NAME.1 = "GCAT_4.6_EX"
INPUT.2 = "GCAT_4.6/multiplePlateAnalysis_sigmoid/output_gcat.fit_2014-11-04_16.28.09.txt"
NAME.2 = "GCAT_4.6_YB"

#  Compare loess fits between different versions of GCAT 4 or loess to sigmoid
#PARAMETERS = list(c("lag.time..hrs","lag.time..hrs","Lag Time"), c("inflection.time..hrs","inflection.time..hrs","Inflection Time"), c("max.spec.growth.rate..log.OD.hr","max.spec.growth.rate..log.OD.hr","Max Specific Growth Rate"), c("achieved.growth.OD","achieved.growth.OD","Achieved Growth, OD"))
#WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.19 GCAT 4/3_GCAT_example_data_analysis"
#INPUT.1 = "/mnt/file.glbrc.org/shared/6.4.1 Bukhman/Public/GCAT4.5 Test/singlePlateAnalysis_loess_compare/output_gcat.fit_2014-11-06_13.20.15.txt"
#NAME.1 = "GCAT_4.6_loess_EX"
#INPUT.2 = "GCAT_4.6/singlePlateAnalysis_loess/output_gcat.fit_2014-11-04_17.15.25.txt"
#NAME.2 = "GCAT_4.6_loess_YB"

#  Specify some wells as empty for the single plate dataset
#EMPTY.WELLS = c(paste(LETTERS[1:8],rep(c("01","06","11","12"),each=8),sep=""), paste(rep(c("A0","H0"),each=7),rep(c(2:5,7:9),2),sep=""),"A10","H10",paste("B0",2:5,sep=""))
EMPTY.WELLS = c() # (multiplate)

setwd(WORK.DIR)

#  Read in the data
in1 = read.table(INPUT.1,header=T,sep="\t")
in1 = subset(in1, well != "")
in2 = read.table(INPUT.2,header=T,sep="\t")
in2 = subset(in2, well != "")

#  Filter out empty wells
in1 = subset(in1, ! well %in% EMPTY.WELLS)
in2 = subset(in2, ! well %in% EMPTY.WELLS)

####  Correlations between growth curve parameters ####
plot.corr = function(in1,param1,in2,param2,title,xlab,ylab) {
  plot(in1[,param1],in2[,param2],main=title,xlab=xlab,ylab=ylab)
  mtext(paste("cor =",round(cor(in1[[param1]],in2[[param2]]),2)))
  abline(0,1)
  filt = is.finite(in1[,param1])
  lines(in1[filt,param1],predict(lm(in2[filt,param2] ~ in1[filt,param1])),lty=3,col=2)
}

for (par in PARAMETERS) {
  plot.corr(in1,par[1],in2,par[2],title=par[3],xlab=NAME.1,ylab=NAME.2)
}


