#Copyright 2012 The Board of Regents of the University of Wisconsin System.
#Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead, 
#Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
#
#This file is part of GCAT.
#
#GCAT is free software: you can redistribute it and/or modify
#it under the terms of the GNU Lesser General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#GCAT is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Lesser General Public License for more details.
#
#You should have received a copy of the GNU Lesser General Public License  
#along with GCAT.  If not, see <http://www.gnu.org/licenses/>.

# Notes by Jason
# 9/07/11


########################################################################
#                                                                      #
#  Function for loading data from tabular format into an object array  #
#                                                                      #
########################################################################
#' Load tabular data
#'
#' This function handles loading data from tabular format (.csv, tab-delimited text or R data frame object)
#' and returns an array of well objects, each filled with raw Time vs. OD data.  
#' It takes single-plate or multiple-plate format data. For single-plate data, 
#'it calls on the function \code{gcat.reorganize.single.plate.data} to rearrange the table before creating the output object. 
#'
#' @param file.name Complete path and file name of a comma-separated values (.csv) file containing growth curve data 
#' in the multiple-plate (long) format.       
#' @param input.data A list of tables representing input files read with \code{read.table}. Used to save time in cases
#' of running multiple analyses on the same dataset. If used, the function will ignore \code{file.name} entirely.
#' @param load.type .csv by default. 
#' @param plate.layout Specifies the layout of the given plate.
#' @param single.plate.ID specifies a plate name for a single-plate read.  If NULL, this is derived from the file name. 
#' @param blank.value Blank OD measurement for uninoculated wells. By default(NULL), the value of the first OD
#'        measurement in each well is used.
#' @param plate.nrow The number of rows in the input files.
#' @param plate.ncol The number of columns in the input files.
#' @param input.skip.lines specifies a plate name for a single-plate read.  If NULL, this is derived from the file name. 
#' @param multi.column.headers The headers of the column when analyzing multiple plates.
#' @param single.column.headers The headers of the column when analyzing a single plate.
#' @param layout.sheet.headers The headers of the layout file.
#' @param silent Surpress all messages.
#' @param start.index Which timepoint should be used as the first one after inoculation?
#' @param single.plate Is the plate single type?
#' 
#' @return A list of well objects.
gcat.load.data = function(file.name = NULL, load.type = "csv", input.data = NULL, single.plate.ID = NULL, 
  plate.layout = NULL,plate.nrow = 8, plate.ncol = 12, input.skip.lines = 0,
  multi.column.headers = c("Plate.ID", "Well", "OD", "Time"), single.column.headers = c("","A1"), 
  layout.sheet.headers = c("Strain", "Media Definition"),
  blank.value = NULL, start.index = 2, single.plate = F, silent = T){

  ########################################################################
  #    Read from .csv, tab-delimited text file or data frame object      #
  ########################################################################
  
	if(is.null(input.data)){
    # Check for bad encoding.
    file.name.cmd = paste('"', file.name, sep = "")
    file.name.cmd = paste(file.name.cmd, '"', sep = "")
    cmd = paste("enca -L none", file.name.cmd)
    sys.msg = system(cmd, intern = TRUE)
    if (length(attributes(sys.msg)) == 1) {
      if (attributes(sys.msg)[1] == 1)
        exception("", "Unrecognized encoding for input file. The file should be in UTF-8.")
    }
    
		# Either read from .csv.
    
    # Skipping temperature column.
    colclasses = c(rep("integer", 1), rep("NULL", 1), rep("integer", 96))
    
		# input.data = read.csv(file.name, colClasses = colclasses, stringsAsFactors=F, skip = input.skip.lines, fileEncoding='UTF-8')
		input.data = read.csv(file.name, stringsAsFactors=F, skip = input.skip.lines, fileEncoding='UTF-8')
		# To remove lines with all the columns as empty or NA
		input.data = input.data[!apply(is.na(input.data) | input.data == "", 1, all), ]
    # Checking for temperature column.
    # Reread the file if temp column is there.
    # MB: Not the best solution.
    if (length(input.data) == 98)
      exception("", "The number of columns in the file is not 97 columns format. Please resubmit a valid file.")
    

		# Determine single plate name if not specified. 
    if (is.null(single.plate.ID)){
      # Split the file name by "." and discard the last member (file extension). 
      single.plate.ID = strsplit(basename(file.name),"\\.")[[1]]
      single.plate.ID = paste(single.plate.ID[-length(single.plate.ID)],collapse=".")
      }
		}

  # Call <gcat.reorganize.single.plate.data> to arrange data from a single plate format file
	if(single.plate)
		input.data = gcat.reorganize.single.plate.data(input.data = input.data, single.column.headers,
      blank.value = blank.value, single.plate.ID = single.plate.ID, plate.nrow = plate.nrow, plate.ncol = plate.ncol, silent=silent)

  ########################################################################
  #     Search for and standardize column headers in input data          #
  ########################################################################
  
   # Go through the specified column headers, determining what their positions are in the 
   # input data frame and if any are missing.
  
   # Get names of column headers in input data
   input.colnames = colnames(input.data)
  
   # Create a list denoting the column numbers in input data that match each of the specified column names, 
   # and a separate list for any missing columns. 
  
   column.matches = c()
   missing.list = NULL
  
	for(i in 1:length(multi.column.headers)){
		if (multi.column.headers[i] %in% input.colnames)
			column.matches[i] = min(which(input.colnames  == multi.column.headers[i]))
		# Take the first column in input file that matches a specified column header.
		else{
			missing.list = c(missing.list, i)
		}
	}

  # If any columns are missing, stop and report error with missing column names
	if (is.vector(missing.list)){
		message = "The following columns:"
		for (i in missing.list)
			message = paste(message, paste("      ", multi.column.headers[i]), sep = "\n")   
		stop(message, "\n were not found in the data file.")
		}

  # Reorder and rename the columns, using the list of matching column numbers from above.
	input.data = input.data[,column.matches]
	names(input.data)[1:4] = c("Plate.ID", "Well", "OD", "Time") 

  # Use 'substring' to split the alphanumeric "Well" field into row (letters) and column (numbers)
	input.data$Well.row = substring(input.data$Well, 0,1)
	input.data$Well.col = as.numeric(substring(input.data$Well, 2))


  ########################################################################
  #        Create an array of well objects with the Time and OD data     #
  ########################################################################
  #
  # Use the by function to split up the data frame into shorter segments by well (row, column and plate)
  
	well.array = by(data = input.data[,c("OD", "Time")], 
    INDICES = list(input.data$Well.row,input.data$Well.col,input.data$Plate.ID), 
    FUN = function(x){data.frame(Time=x$Time, OD=x$OD,stringsAsFactors = F)}, simplify = F)

  
  # Then apply the function <well> (found in well.class) to each member to create a well object
  well.array = aapply(well.array,function(x){well(x$Time,x$OD)})

  # Differentiate any duplicate plate names in the array's dimnames 
	new.dimnames = dimnames(well.array)
  for (i in length(new.dimnames[[3]]):1){
		if (any(new.dimnames[[3]][-i] == new.dimnames[[3]][i]))
			new.dimnames[[3]][i] = paste("another_", new.dimnames[[3]][i], sep = "")	
		}
	dimnames(well.array) = new.dimnames
	
  # Copy the plate/row/column names found in the dimnames into the array objects themselves (use "position" slot)
	for(plate in unique(dimnames(well.array)[[3]])){
		for (col in unique(dimnames(well.array)[[2]])){
			for(row in unique(dimnames(well.array)[[1]])){
				well.array[[row,col,plate]]@position = c(plate=plate,row=row,col=col)
				}
			}
		}
		
  ########################################################################
  #       Add plate layout information to well array                     #
  ########################################################################
  
  # Use the <plate.layout> object to add media and strain information to the "well.info" slot of each well
  # Also set the value of the parameter <empty.well> in slot "curve.par" to T for empty wells. 
  ########################################################################
  #       Add plate layout information to well array                     #
  ########################################################################
  
  # Use the <plate.layout> object to add media and strain information to the "well.info" slot of each well
  # Also set the value of the parameter <empty.well> in slot "curve.par" to T for empty wells. 
  

  # If <plate.layout> is not provided, do not add strain information, and assume all wells are inoculated. 
  if(is.null(plate.layout)){  
     plate.layout = data.frame(Row=rep(PLATE.LETTERS[1:plate.nrow],plate.ncol),Column=rep(1:plate.ncol,each=plate.nrow),rep("Unknown Strain",96),rep("Unknown Media",96))
     colnames(plate.layout) = c("Row", "Column", layout.sheet.headers)
     }  
  else
    if(!silent) cat("\n\t\tusing plate layout to fill well info.")
    
	for(plate in unique(dimnames(well.array)[[3]])){
		for (col in unique(dimnames(well.array)[[2]])){
			for(row in unique(dimnames(well.array)[[1]])){
        well = well.array[[row,col,plate]]
        # For each well on each plate, find the corresponding row in <plate.layout>. 
        # If <plate.layout> refers to specific plates, then use those to find the correct row. 
        #  Otherwise, generalize across all plates. 
        if ("Plate.ID" %in% names(plate.layout)) 
          layout.row.number = which(plate.layout$Column==well@position["col"] & 
                                      plate.layout$Row==well@position["row"] & 
                                      plate.layout$Plate.ID==well@position["plate"] )  
        else 
          layout.row.number = which(plate.layout$Column==well@position["col"] & 
                                      plate.layout$Row==well@position["row"])
        
        # Error if either no rows or more than one row matches the well
        if (length(layout.row.number) != 1)
          stop("Incorrectly formatted plate layout! check names of columns, rows, and plates (if applicable).")
        
        # Add any additional columns to the well's "well.info" slot
        well.info = plate.layout[layout.row.number,!(names(plate.layout) %in% c("Row","Column","Plate.ID",layout.sheet.headers))]
        
        # Fix the column name issue if there is only one additional entry. 
        if(length(well.info) == 1){
          well.info = data.frame(well.info,stringsAsFactors=F) 
          names(well.info) = names(plate.layout)[!(names(plate.layout) %in% c("Row","Column","Plate.ID",layout.sheet.headers))] 
          }                    
        well@well.info = as.list(well.info)
        
        well@well.info$Strain = plate.layout[layout.row.number, layout.sheet.headers[1]]
        well@well.info$Media = plate.layout[layout.row.number, layout.sheet.headers[2]]
        
        # Set <empty.well> parameter in slot "curve.par" accordingly 
        well@curve.par$empty.well = (plate.layout$Strain[layout.row.number] == "Empty")     
        well.array[[row,col,plate]] = well
				}
			}
		}
  
  #  Set start index value in each well
  well.array = aapply(well.array, function(x,start.index) { x@start.index = start.index; x }, start.index)
  
  ########################################################################
  #                 Return values to R                                   #
  ######################################################################## 
  #                           
	# Console output if desired, return the completed well array.
	if (!silent)
		cat("\n\t", dim(well.array)[[3]], "plates added to array from", file.name)
	return(well.array)
	}



########################################################################
#                                                                      #
#  Reorganize data from single-plate input format before reading       #
#                                                                      #
########################################################################
#  Reorganize data from single-plate input format before reading       #
#
# This function reorganizes the data frame from a single-plate format file. 
# @param   input.data - data frame read straight from a single-plate format data file. 
# @param   single.plate.ID - specifies a plate name for a single-plate read, since none is given in the single-plate format.
#                       The plate will be named Plate_1 unless otherwise specified. 
#                       
gcat.reorganize.single.plate.data = function(input.data, blank.value = NULL, single.column.headers, single.plate.ID = "Plate_1", 
                                             plate.nrow = 8, plate.ncol = 12, silent=T){
 
  ########################################################################
  #   Standardize the formatting and search for specified column names   #
  ######################################################################## 
  #                        
  # Locate the first and last rows from the table and return errors if not defined 
  # Note: this only works if the time column is the first column
  
	header.row = min(which(input.data[,1] == single.column.headers[1])) 
  if (length(header.row) != 1 | is.infinite(header.row))
    stop("Could not locate header row in input file!")
    
  # The last row: where column 2 starts to be blank, or the total number of rows, whichever is smaller 
  extra.rows.start = min(which(input.data[-(1:header.row),2] == ""), which(is.na(input.data[-(1:header.row),2])), nrow(input.data[-(1:header.row),]))
  if (length(extra.rows.start) != 1 & is.infinite(extra.rows.start))  
    stop("Could not locate last row in input file!")

  # Use header row to rename the columns, then cut off extra rows (including the ones above header)
	names(input.data) = as.character(unlist(input.data[header.row,]))
  input.data = input.data[(header.row+1):(header.row+extra.rows.start-1),]
  
  # Select columns which contain OD values for wells
  OD.columns = which(names(input.data) %in% gsub(" ", "", paste(rep(PLATE.LETTERS[1:plate.nrow], each = plate.ncol), rep(formatC(1:plate.ncol, digits = log(plate.ncol, 10)), plate.nrow), sep = ""), fixed = TRUE))
  
  # Time column: allow for multiple matches to the name (since it's usually blank) but assume it's the first one
	Time.column = which(names(input.data) == single.column.headers[1])
	if (length(Time.column) != 1){
    if(!silent) cat("No unique time column in input.data file! Using the first one encountered.")
		Time.column = min(Time.column)
	}
	
	# Reformat input.data and select columns that are supported by GCAT i.e. Time column and OD columns 
	Supported.columns = c(Time.column, OD.columns)
	input.data = input.data[,Supported.columns]
	
  # First well column (default A1): only allow for one match.	
	Well.column.start = which(names(input.data) == single.column.headers[2])
	if (length(Well.column.start) != 1)
		stop("No unique start point for well columns in input.data file!")

  # If the time column was found, rename it "Time" and reformat it into a numeric value
  # Adjust the blank measurement timestamp to -1 seconds if there is one

  names(input.data)[Time.column] = "Time"
  
  # Note: Some single plate screens have timepoints ending with "s" for seconds. 
  # This line removes the "s" while maintaining general compatibility. 
	input.data$Time = unlist(strsplit(input.data$Time, "s"))

  # If <blank.value> is NULL (default - takes the first OD as the blank reading), then the first timepoint can labeled something non-numeric. 
  # In that case, rename it to match the first real timepoint minus one. 
  # when user input blank value, Blank timepoint i.e. input.data$Time[1] == Blank, labeled as "Blank" from data input file
  # It also should rename it to match the first real timepoint minus one. 
  if(is.null(blank.value) || is.na(as.numeric(input.data$Time[1])))
    input.data$Time[1] = as.numeric(input.data$Time[2]) - 1
    
  ########################################################################
  #          Start to fill the reformatted data frame                    #
  ######################################################################## 
  
  # If all columns are present, make a list of all the wells.
	well.list = paste(rep(PLATE.LETTERS[1:plate.nrow], each = plate.ncol), rep(formatC(1:plate.ncol, digits = log(plate.ncol, 10), flag = "0"), plate.nrow), sep = "")

  #	Duplicate the well names times the number of time measurements in each well	
  Well = rep(well.list, each = length(input.data[,Time.column]))	
		
  # Duplicate <plate.name> times the length of the entire output 
  Plate.ID = rep(single.plate.ID, length(Well))

  # Duplicate the time column times the number of wells and convert to numeric
	Time = as.numeric(rep(input.data[,Time.column], times = length(Well.column.start:ncol(input.data))))

  # Append OD measurements from each well together and convert to numeric
	OD = c()
	for (i in Well.column.start:ncol(input.data)){
		OD = as.numeric(c(OD, input.data[,i]))
		OD = unlist(OD)
		}

  # Fill and return the data frame containing the above four vectors.
	output = data.frame(Plate.ID, Well, OD, Time)	
	
  # Include any extra columns that were not Time or OD measurements?
	for(i in (1:length(names(input.data)))[-c(Time.column,Well.column.start:ncol(input.data))]){
		new.column = data.frame(rep(input.data[,i], length(Well.column.start:ncol(input.data))))
		names(new.column) = names(input.data)[i]
		output = cbind(output, new.column)
		}	
	return(output)
}


########################################################################
#                                                                      #
#    Function to combine two well array datasets by plate              #
#                                                                      #
########################################################################
# ----------------------------------------------------------
#'    Function to combine two well array datasets by plate
#'    
#' This function can append together arrays created using <load.data> 
#' 
#' @param ... any number of array objects as long as they are all output straight from <load.data> 
#'
gcat.append.arrays = function(...){

  # Transfer arrays to a list
	args.arrays = list(...)
	first.array = args.arrays[[1]]
	first.dims = dim(first.array)
  plate.nrow = args.arrays[[4]]
  plate.ncol = args.arrays[[3]]
  input.arrays = list(args.arrays[[1]], args.arrays[[2]])
	for (i in 2:length(input.arrays)){
		next.array = input.arrays[[i]]
		next.dims = dim(next.array)
	
    # Check to make sure the arrays have proper dimensions for 96-well plate data
    if (!(all(c(first.dims[1], next.dims[1]) == plate.nrow) & all(c(first.dims[2], next.dims[2]) == plate.ncol)))
			stop("Data should have dimensions (",plate.nrow,",",plate.ncol,",n)!")
		
    # If dimensions are alright, append dimensions and dimension names	
    new.dims = c(plate.nrow,plate.ncol,first.dims[3]+next.dims[3])
		new.names = dimnames(first.array)
		new.names[[3]] = c(new.names[[3]], dimnames(next.array)[[3]]) 

    # Differentiate duplicate names
		for (i in length(new.names[[3]]):1){
			if (any(new.names[[3]][-i] == new.names[[3]][i]))
				new.names[[3]][i] = paste("another_", new.names[[3]][i], sep = "")
			}
	
		# Create a new array
    new.array = c(first.array, next.array)
		dim(new.array) = new.dims
		dimnames(new.array) = new.names
		
    # Update plate name in well objects
		for (i in 1:length(unlist(new.array)))
			new.array[[i]]@position[1] = new.names[[3]][floor((i-1)/96)+1]
		
		# Loop until complete
		first.array = new.array
		first.dims = dim(first.array)
		}
	return(new.array)
	}


########################################################################
#                                                                      #
#     Convert timestamps to hours from start and sort timepoints       #
#                                                                      #
########################################################################
#'     Convert timestamps to hours from start and sort timepoints
#
#' This function acts on a single well and modifies the raw data stored in slot "screen.data"
#' 
#' @param input.well an object of class well
#' @param time.input specifies time format in the input. allowed values are "%S", for seconds, "%d", for days, or anything complying with 
#' ISO C / POSIX standards; see <strptime>.
#' @param start.index which timepoint should be used as the starting time at inoculation?
#'
#' @details note: reading directly from excel to R results in timestamps being converted to days.
gcat.start.times = function(input.well, time.input, start.index = 2) {  
 
  if(start.index > length(input.well))
    stop("Start index is greater than length of well!")
 
  # If using a numeric time format, simply multiply times by the appropriate conversion factor
	# Conversion factor should be supplied to convert timestamps to hours. For example, 
  # <time.format> should be equal to 1/3600 if "time" is in seconds, 24 if "time" is in days, etc.

  time.format = time.input # Set default constant from rails user input
  
  if (is.numeric(time.format))
		input.well@screen.data$Time = (input.well@screen.data$Time - min(input.well@screen.data$Time)) * time.format
  else{
  # Otherwise, convert timestamps from ISO C / POSIX to numeric values representing seconds (since the dawn of time?) and subtract out the initial value. 
		rtimes = input.well@screen.data$Time
      ptimes = strptime(as.character(rtimes),time.format)
		ctimes = as.POSIXct(ptimes)
		int.times = unclass(ctimes)
    # Time will be in seconds, convert to hours by dividing by 3600
		input.well@screen.data$Time = (int.times - min(int.times))/3600
		}
	# Sort raw data by timestamp and return the input.well
	input.well@screen.data = input.well@screen.data[order(input.well@screen.data$Time),]
  
  input.well@screen.data$Time = input.well@screen.data$Time - input.well@screen.data$Time[start.index]
  
  if(all(is.na(input.well@screen.data$Time)))
    stop("Error in <time.format>.")  
  rownames(input.well@screen.data) = NULL
	return(input.well)
  }	
