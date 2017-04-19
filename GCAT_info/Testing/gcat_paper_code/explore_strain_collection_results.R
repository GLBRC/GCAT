#  Explore strain collection results:
#  1) Variety of sigmoid models
#  2) Correlation between sigmoid and loess

WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.06 GCAT paper/2014 Bioenergy Research/Supplementary data/4 strains in 12 media/GCAT_outputs"
SIGMOID.FILE = "sigmoid/output_gcat.fit_2014-10-03_16.40.42.txt"
LOESS.FILE = "loess_0.1/output_gcat.fit_2014-10-03_16.44.11.txt"
EMPTY.WELLS = c(paste(LETTERS[1:8],rep(c("01","06","11","12"),each=8),sep=""),
                paste(rep(c("A0","H0"),each=7),rep(c(2:5,7:9),2),sep=""),
                "A10","H10",
                paste("B0",2:5,sep=""))
#WORK.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/GCAT 4/2_Piotrowski_data_analysis/working_data/GCAT_4.3_results/ACSH686 GWASv1"
#SIGMOID.FILE = "sigmoid/output_gcat.fit_2014-09-25_09.43.27.txt"
#LOESS.FILE = "loess_0.1/output_gcat.fit_2014-09-25_09.45.57.txt"
#EMPTY.WELLS = c(paste(LETTERS[2:7],"01",sep=""),
#                paste(rep(c("A0","H0"),each=9),rep(c(1:9),2),sep=""),
#                paste(rep(c("A","H"),each=3),rep(10:12,2),sep=""),
#                "B12","G12")


#  Load data
setwd(WORK.DIR)
sigmoid = read.delim(SIGMOID.FILE,nrows=96)
loess = read.delim(LOESS.FILE,nrows=96)

#  Filter out empty wells
sigmoid = subset(sigmoid, ! well %in% EMPTY.WELLS)
loess = subset(loess, ! well %in% EMPTY.WELLS)

#  Summarize sigmoid models
summary(sigmoid$model)

#  Correlation between growth curve parameters computed using sigmoid and loess models
cor(sigmoid$lag.time,loess$lag.time)
cor(sigmoid$spec.growth,loess$spec.growth)
cor(sigmoid$tot.growth,loess$tot.growth)

plot.corr = function(loess,sigmoid,param,title) {
  plot(loess[,param],sigmoid[,param],xlab="loess",ylab="sigmoid",main=title)
  abline(0,1)
}

plot.corr(loess,sigmoid,"lag.time","Lag Time")
plot.corr(loess,sigmoid,"spec.growth","Specific Growth Rate")
plot.corr(loess,sigmoid,"tot.growth","Total Growth")
