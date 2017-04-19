###################################################################################
# Script to calculate the median of the three background measures (F12, G11 & G12)#
# Modify those if background was contaminated or data is not correct              #
# Background will be sustracted from raw_data and a new file will be created      #
# usage: R --vanilla --args infile outfile < tecan_back_sustr.R                   #
# By David Peris {Genetics Lab, Biotechnology Center UW-Madison, WI}              #
###################################################################################

 arguments = as.character(commandArgs(trailingOnly = TRUE)); #creates a list where arguments will be included
 infile = arguments[1]; #The first argument in the command line will indicate which is the infile
 outfile = arguments[2]; #The second argument will be used for the output file name
 
 #Our infile will be read
 raw_data = read.table(infile, sep="\t", dec=".", header = TRUE); 
 
 
 #Creates a list of median values from the three blank columns
 i = 1
 colnames(raw_data)[1] = "Time"
 raw_data$Blank = NA
 #New column with data that will be used to remove background
 while (i <= length(raw_data$F12)) {
	raw_data$Blank[i] = median(c(raw_data$F12[i], raw_data$G11[i], raw_data$G12[i])); #change it to indicate your blank wells
	i = i + 1 #Prepare for the next iteration until i is equal the lenght of F12;
	}
 
 #We will remove the background from each column
 i = 2
 while (i < length(raw_data)) {
	raw_data[i] = raw_data[i] - raw_data$Blank;
	i = i +1;
	}
 
 #We will remove the Blank data
 raw_data$Blank = NULL;
 
 write.table(raw_data, file = outfile, sep = "\t", row.names = FALSE);
