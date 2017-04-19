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

########################################################################
########################################################################
#' Normalize OD readings for an entire array of well objects
#'
#' @details
#'  Note: This function does not write any new OD values to the well objects in the array - it only 
#'   fills the "norm" slot of each well object in the array with a value that will be subtracted 
#'   from all OD measurements when returning data from the wells using the function <data.from> (see well.class.R) 
#'   For "average.layout" the function instead of filling "norm" slot writes a list "averagedTimestamp" in screen.data
#'   This averagedTimestamp is a list of average of only Empty wells as defined in layout file for each timestamp
#'
#'    These functions make use of <raw.data> which simply returns the raw time and OD of a well (also see well.class.R)
#'  
#'  note this is the only normalization function that acts on an entire array instead of an individual well.
#'  
#'  normalize.method settings: 
#'  \describe{
#'  \item{default}{subtracts the blank OD (either specified by <blank.value> or taken from the first timepoint as default)  
#'  of each well from all timepoints in that well}
#'  \item{average.blank}{subtracts the mean of all first OD timepoints on a plate from all timepoints in all wells on that plate}
#'  \item{average.first}{takes the mean of the difference between the OD of the specified <start> timepoint and the first timepoint of all wells on a plate
#'                     and subtracts this value from all timepoints in all wells on that plate}
#'  \item{average.layout}{Takes an average of all ODs at each timestamp taking into consideration only "Empty" wells as defined in layout file
#'                     It then stores the list of average timestamps as averagedTimestamp in screen.data of every well}
#'  \item{anything else}{do nothing}
#'  }
#'  
#' @param  normalize.method see Details 
#' @param  well.array an array of well objects. 
#' @param  blank.value user can enter a blank OD measurement for uninoculated wells. if NULL, defaults to the value of the first OD measurement of each well. 
#' @param  start.index which timepoint should be used as the first one after inoculation (defaults to the 2th one)
#' @param  add.constant add a numeric constant to all timepoints in all wells. 
normalize.ODs = function(well.array, normalize.method = "default", blank.value = NULL, start.index = 2, add.constant = 1){
  
  if (normalize.method == "default"){
    well.array = aapply(well.array, function(well, blank.value){
      # Use the blank OD value if specified; otherwise, get it from the first OD timepoint. 
      if(is.null(blank.value)) blank.value = raw.data(well)[1,2]
      # Set the blank OD (minus the constant to be added) to the "norm" slot of each well.
    	well@norm = blank.value - add.constant
    	return(well)}, blank.value)
  }
  else if (normalize.method == "average.blank"){ 
    # Use the blank OD value if specified; otherwise, get it from the first OD timepoint.
		blank.ODs = unlist(aapply(well.array, function(well, blank.value){
      if(is.null(blank.value)) blank.value = raw.data(well)[1,2]
      return(blank.value)}, blank.value))
		plate.IDs = unlist(aapply(well.array, plate.name))
		blank.averages = tapply(blank.ODs, plate.IDs, mean)
	  # Set this value (minus the constant to be added) to the "norm" slot of each well. 
		well.array = aapply(well.array, function(well){
			well@norm = blank.averages[plate.name(well)] - add.constant
			return(well)})
	}
	else if (normalize.method == "average.first"){
	  # Find the mean difference between starting OD (timepoint specified by <start>) and blank OD (first timepoint) for each plate
    # Use the blank OD value if specified; otherwise, get it from the first OD timepoint.
    blank.ODs = unlist(aapply(well.array, function(well, blank.value){
      if(is.null(blank.value)) blank.value = raw.data(well)[1,2]
      return(blank.value)}, blank.value))
		first.ODs = unlist(aapply(well.array, function(well) raw.data(well)[start.index,2]))
		plate.IDs = unlist(aapply(well.array, plate.name))
	  blank.averages = tapply(first.ODs-blank.ODs,plate.IDs,mean)
	  # Set this value (minus the constant to be added) to the "norm" slot of each well.
		well.array = aapply(well.array, function(well){
			well@norm = raw.data(well)[start,2] - blank.averages[plate.name(well)] - add.constant  
			return(well)})
	}	
  else if (normalize.method == "average.layout"){
    # Works only for single plate
    # For each timestamp our blank value is the average of OD 
    #         of only those wells that are marked Empty in layout file
    
    # blank.ODs will be a list of all OD
    # The ODs at every timestamp for all non Empty wells will be -25.0000
    blank.ODs = unlist(aapply(well.array, function(well){
      if(well@well.info$Strain[1] == "Empty") 
        OD = well@screen.data$OD
      else
        OD = rep(-25, length(well@screen.data$OD))
      return(OD)}))
    
    # Create back the matrix of timestamp as our rows and wells as columns
    #           with values of non empty wells at each timestamp as -25.0000
    Input_OD_Data<-matrix(unlist(blank.ODs),ncol=length(well.array),byrow = FALSE)
    
    # Take the average of only Empty wells for each timestamp
    avgBlanks = unlist(apply(Input_OD_Data, 1, function(x){
      x=setdiff(x, c(-25.0000))
      blank.average = mean(x) - add.constant
      return(blank.average)}))

    # Store in each well's screen data the averagedTimestamp
    well.array = aapply(well.array, function(well, avgBlanks){
      #well@screen.data$averagedBlankValues = avgBlanks
      well@norm = avgBlanks
    return(well)},avgBlanks)
    # Set this value (minus the constant to be added) to the "norm" slot of each well. 
    #well.array = aapply(well.array, function(well){
     # well@norm = blank.average - add.constant
      #return(well)})
  }
	else{
    # Simply set the negative constant to be added to the "norm" slot of each well. 
		well.array = aapply(well.array, function(well){
			well@norm = - add.constant
     			return(well)})
	}
  if(is.null(blank.value) && !(normalize.method == "average.layout"))
    well.array = aapply(well.array, remove.points, 1)
  return(well.array)
}

########################################################################
########################################################################

# Must include this so that the checking process will not complain about
# inconsistency S3 generic/method. Though I don't know why.
# S3 generic
# @seealso \code{\link{transform.ODs}}
transform <- function(input.well, ...) {
  UseMethod("transform")
}

#' Log-transform OD readings for a single well object 
#'
#' This function adds a "log.OD" column to the "screen.data" slot of a well object with log-transformed data.  
#' The raw data is kept intact.  
#' It also checks to see if any of the raw OD values (before a certain timepoint) is below the blank OD.  
#' This can be disastrous for the log(OD) transform.  
#' 
#' @param input.well an object of class well 
#' @param use.log gets added to the "use.log" slot of the well object. this will determine whether the log-transformed data  
#'              or raw normalized data is returned using the function \code{data.from}.    
#' @param blank.value user can enter a blank OD measurement for uninoculated wells. if NULL, defaults to the value of the first OD measurement of each well.  
#' @param start.index which timepoint should be used as the first one after inoculation (defaults to the 2th one) 
#' @param negative.OD.cutoff if any ODs below the specified blank value are detected before this index timepoint, the entire well is discarded.
#' @param constant.added similar to added.constant.
#' @param normalize.method To specify if we are using average.layout method to set our blank value at each timestamp as average of
#               OD values of only those wells which are marked Empty
#' @param ... Additional arguments for this function.
transform.ODs = function(input.well, use.log = T, blank.value = NULL, start.index = 2, negative.OD.cutoff = 10, constant.added = 1.0, normalize.method = normalize.method, ...){
 
  # The default value for the log-transformed ODs will be NA. Valid values will be filled in. 
  log.OD = rep(NA, length(input.well))
  OD = raw.data(input.well)[,2]
  
    
	# Use the blank OD value if specified; otherwise, get it from the first OD timepoint.
  if(is.null(blank.value))
      blank.value = OD[1]

  else if(normalize.method == "average.layout")
      blank.value = input.well@norm
    
  # Remove any points from the analysis that weren't already removed and fall below the blank value (using <remove.points> below)
  OD[input.well@screen.data$Remove] = NA
  
  negative.points = which(OD  + 0.2 * constant.added < blank.value)
  if(length(negative.points) > 0)
	   input.well = remove.points(input.well, negative.points)
    
  # If any points fall below the blank value by more than 0.2 * <constant.added> and before the cutoff index <negative.OD.cutoff>, remove the well from analysis. 
  # First adjust the cutoff to compensate for curves that don't start at timepoint 1
  negative.OD.cutoff = negative.OD.cutoff + start.index - 1
  
  if(any(negative.points <= negative.OD.cutoff)){
    input.well = remove.points(input.well, rep(T,length(input.well)))
    input.well@add.info = paste("ODs at timepoint(s)", paste(negative.points[negative.points <= negative.OD.cutoff],collapse=" "), "were below blank OD; well discarded")
  }

  # Take the natural log of the rest of the OD values (after subtracting the normalization value)
  if(normalize.method == "average.layout")
    log.OD[which(OD > input.well@norm)] = log(OD[which(OD > input.well@norm)] - input.well@norm[which(OD > input.well@norm)])
  else
    log.OD[which(OD > input.well@norm)] = log(OD[which(OD > input.well@norm)] - input.well@norm)
  
	# Add a column to the "screen.data" slot of the well
	input.well@screen.data$log.OD = log.OD	
	# Update the "use.log" slot of the well 
	input.well@use.log = use.log	

	return(input.well)
  
}

########################################################################
########################################################################
#    Remove timepoints from the analysis but not from the raw data     #
#
# @details 
#   Removes timepoints from further analysis. Does not remove them from the raw data;
#   instead, this function creates or updates the Remove column in slot "screen.data" of the well which dictates whether 
#   individual timepoints are returned using the <load.data> function. 
#
#   parameter \emph{points} can be a vector containing:
#   \itemize{
#   \item{any combination of positive and negative integers 
#      the timepoints at indices corresponding to positive integers will be set to be removed.
#      the timepoints at indices corresponding to negative integers will be be re-added if they were previously set to be removed.}
#   \item{a single zero, which resets all timepoints (nothing will be removed)}
#   \item{a logical vector to replace the Remove column and which will be cycled along the length of the timepoints.}
#   }
#
remove.points = function(input.well, points){
  # Copy the Remove column or create a new one if it doesn't yet exist
	if (is.null(input.well@screen.data$Remove))
		Remove = rep(F, length(input.well))
	else
		Remove = input.well@screen.data$Remove

  # If <points> is a logical vector, recycle it along the length of Remove 
	if (length(points[!is.na(points)]) != 0){
	# Separate positive and negative integers
  	if (is.logical(points) & !any(is.na(points)))
  		Remove = rep(points,length.out=nrow(input.well@screen.data))	
 		else{
    	pos = points[points > 0]
    	neg = -points[points < 0]
    	Remove[pos] = T
    	Remove[neg] = F  
    		  	
    # If <points> contains only zeros, reset the Remove vector to all F 	
    	if (all(points == 0))
    		Remove[1:length(Remove)] = F
   		}
    }
  # replace the Remove column
	input.well@screen.data$Remove = Remove
	input.well
	}

#   Add a column y = OD - blank.value for each well@screen.data
#   switch Remove flag to TRUE if y < 0.
subtract.blank = function(well.array, blank.value = NULL) {
  well.array = aapply(well.array, function(well, blank.value){
    if (is.null(blank.value))
      blank.value = well@screen.data$OD[1]
    
    well@screen.data$y = well@screen.data$OD - blank.value
    well = remove.points(well, well@screen.data$y < 0)
    #newOD = well@screen.data$y
    #oldOD = well@screen.data$OD
    #well@screen.data$OD = newOD
    #well@screen.data$y = oldOD
    return(well)}, blank.value)   
}



