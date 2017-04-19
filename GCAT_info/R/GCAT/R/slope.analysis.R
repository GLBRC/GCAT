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
#    Estimate the growth curve slope at each timepoint of a well
#    
#    @details
#   uses the functions <data.from> and <well.name> (see well.class.R) 
#   adds estimated slopes as a new column to the "screen.data" slot
#
calculate.slopes = function(input.well, silent = T){
  # Get the growth curve data (excluding removed points, but not excluding points marked as tanking)
	growth.data = data.from(input.well, remove = T, remove.tanking = F)
	x = growth.data[,1]
	y = growth.data[,2]
	
	# Get a list of timepoint indices 
	indices = as.numeric(rownames(growth.data))
	
  # Default slope is NA, values will be filled in as they are calculated
  slopes = rep(NA, length(input.well))

	for (i in 1:length(x)){  
		if (i == 1)
			slopes[indices[i]] = NA
    # Calculate the slope of the line drawn from each point to the point preceding it. 
		else
			slopes[indices[i]] = (y[i] - y[i-1])/(x[i] - x[i-1])
		}
  # Add a Slope column to the "screen.data" slot 
	input.well@screen.data$Slope = slopes
	if (!silent)
		cat("slopes filled for", input.well@position[1], well.name(input.well), "\n")
	return(input.well)
  }
  
  
########################################################################
########################################################################
#' Use slope estimates to check growth curves for tanking and OD jumps #
#'
#' @details
#' Uses the functions <data.from> and <well.name> (see well.class.R) 
#'  
#' Fills the "curve.par" slot in the well with the starting index of tanking (NA if none is found)
#' 
#' Changed default values to parameters to account for settling
#' 
#' @param input.well object of class \code{well} to check the slopes for 
#' @param remove.jumps should the program remove OD jumps? default F (just report them) - 
#'                  should be set to T if data contains distinct jumps in OD that need to be eliminated 
#'                  otherwise, this might be too stringent and will result in loss of data.  
#' @param check.start which timepoint should checking for jumps and tanking start at? this is included because early timepoints can be unstable.
#' @param fall.cutoff what downward slope should constitute a fall in OD? 
#' @param jump.multipliers multipliers of fall.cutoff that determine whether a curve jumps up or down (see methods 1 and 2, below)
#' @param tank.limit how many timepoints in a row can have falling slopes until the curve is marked as tanking?
#' @param tank.cutoff what proportion of the maximum OD can the curve go below until it is considered tanking?
#' @param silent output to R console?
#' @param draw plot growth curve with curve checking details? 
#'
check.slopes = function(input.well, check.start = 22, fall.cutoff = -.0025, remove.jumps = F,
			jump.multipliers = -c(15, 500, 10), tank.cutoff = 1.0, tank.limit = 6, silent = T, draw = T){

  if (!silent)
    cat("\nNow checking slopes for", plate.name(input.well), well.name(input.well))

	# Get estimated slopes and untransformed points from the well 
  slopes = input.well@screen.data$Slope
	x = data.from(input.well, remove=F, remove.tanking=F)[,1]
	y = data.from(input.well, remove=F, remove.tanking=F)[,2] 
	
	# Get a list of indices with valid slope estimates
	indices = (1:length(input.well))[!is.na(slopes)]

  # Do not report tanking or check for jumps if there are fewer valid points than the number needed to detect it
	if (length(indices) < tank.limit){
		input.well@curve.par$tanking.start = NA
		if (!silent)
			cat("...does not have enough points to check.\n")
		return(input.well)
	}
	
  #######################################################################################
  # Create indicator variables and recalculate cutoffs based on timepoint density.      #
  #######################################################################################
  #

  # Create <slope.indicator>, a vector of indicator variables for each timepoint with a valid slope estimate. 
	slope.indicator = rep(NA, length(slopes))
	
	# Calculate the mean time difference between two timepoints (this typically doesn't vary too much)
  time.diff = mean(diff(x[-1]))
  
	# Use the mean time difference to recalculate what should constitute a fall in OD using <fall.cutoff> (which should be a proportion)
	#    Honestly I don't remember why the fifth root thing is in here...this is probably going to be revised later. 
  fall.cutoff = fall.cutoff * time.diff ^ (1/5) / 0.9506785
  	
	# Recalculate stringency parameters for jump detection based on spread of timepoints
	jump.cutoffs = jump.multipliers* fall.cutoff 

  # Recalculate tanking limit based on spread of timepoints
	tank.limit = round(tank.limit / time.diff ^ (1/5) * 0.9506785)

	# Cycle through the indices of input.wells with valid slope estimate
	counter = 0

  for(j in 1:length(indices)){
  #######################################################################################
  #  Method #1 for finding OD jumps: compare the slope estimate of each point to the    #
  #                                  ones for the closest surrounding points.           #
  #######################################################################################
    # Get indices of the two closest surrounding timepoints with valid slope estimates. 
		if (j == 1)
			prev.i = indices[2]
		else
			prev.i = indices[j-1]
		if (j == length(indices))
			next.i = indices[length(indices) - 1]
		else
			next.i = indices[j+1]
		i = indices[j]
		
		# How the program determines a jump up:
		#   If slope estimate of current timepoint is larger than <jump.cutoffs[2]> times the highest surrounding slope estimate plus <jump.cutoffs[1]> 
		#   Add a "2" to the indicator variable
		# How the program determines a fall:
		#   If slope estimate of current timepoint is more negative than <fall.cutoff> 
		#   Add a "5" to the indicator variable
		# How the program determines a jump down:
		#   If slope estimate is lower than <fall.cutoff> AND is smaller than <jump.cutoffs[2]> times the lowest surrounding slope estimate minus <jump.cutoffs[1]> 
		#   Add a "6" to the slope indicator variable
		#
		# If none of these are true, add a "0" to the indicator variable
		
		if (slopes[i] > jump.cutoffs[2] * max(c(slopes[next.i],slopes[prev.i]),0) + jump.cutoffs[1])
			slope.indicator[i] = 2
		else if (slopes[i] < fall.cutoff){
			slope.indicator[i] = 5
			if (slopes[i] < jump.cutoffs[2] * min(c(slopes[next.i],slopes[prev.i], 0)) - jump.cutoffs[1])
			 slope.indicator[i] = 6	
			}
		else
			slope.indicator[i] = 0


  #######################################################################################
  #  Method #2 for finding OD jumps: see if each point lies close to a line drawn       #
  #                                  between the closest surrounding points.            #
  #######################################################################################
  #
  # Use <counter> variable to track the location of each point. If two subsequent points lie farther 
  # away than the cutoff from their respectively drawn lines AND are on different sides, then count that as a jump. 
  
		if (j > 1 & j < length(indices)){
		
		  # Make equation (y=mx+b) for line drawn between two surrounding points
			m = (y[next.i] - y[prev.i])/(x[next.i] - x[prev.i])
			b = y[prev.i] - m * x[prev.i]		
			
      # Estimate y from that line. Points will be judged by how much their true y value deviate from this estimate.
      
      # calculate b for perpendicular line from observed point to drawn line (slope is -1/m)
      b2 = y[i] + x[i]/m

      # solve equation for intersection to determine the shortest Euclidean distance between the point and line.
      # assign a sign to the distance based on the vertical distance. 
      est.x = (b2 - b) / (m + 1/m)
      est.y = est.x * m + b
      #est.y = m * x[i] + b
      #est.x = x[i]
      
      if(m != 0)
        point.distance = sqrt((y[i]-est.y)^2 + (x[i]-est.x)^2) * sign(y[i]-est.y)
      else # horizontal case
        point.distance = y[i] - b
        
      #print(paste(i, point.distance, slopes[i], jump.cutoffs[2] * max(c(slopes[next.i],slopes[prev.i]),0) + jump.cutoffs[1], point.distance > jump.cutoffs[3])) 
      
			color = "gray30"
			# If the true point exceeds that estimate by more than <jump.cutoffs[3]>, update <counter> to positive.
			#  if the counter weas previously negative, mark this as a jump up. 
			if (point.distance > jump.cutoffs[3]){
				if (counter == -1){
					slope.indicator[i] = 2
					color = "red"
					}
				counter = 1
				}
			# If the true point is under that estimate by more than <jump.cutoffs[3]>, update <counter> to negative.
			#  if the counter was previously positive, mark this as a jump down. 
			else if (point.distance < -jump.cutoffs[3]){
				if (counter == 1){
					slope.indicator[i] = 6
					color = "red"
					}
				counter = -1
				}
			# If the true point lies within <jump.cutoffs[3]> of that estimate, update <counter> to zero. 
			else 
				counter = 0
   	
			if(draw)
        # Graphic representation: draw each line used in Method #2 as a dotted line, 
        #    and highlight in red if a jump was detected
				lines(x[c(prev.i, next.i)], y[c(prev.i, next.i)], lty = 2, col = color)
			}
		}

  #######################################################################################
  #  Check for tanking by looking for unbroken series of points with falling slopes.    #
  #######################################################################################
  #
  #  Cycle through <slope.indicator>, adding to <tank> until the end of the curve or until <tank> reaches the <tank.limit>

	tank = 0
	i = 1
	
	while(i < length(slope.indicator) & tank < tank.limit){
	# If a fall was not detected, reset <tank> to 0. 
		if (is.na(slope.indicator[i]))
			tank = 0    
    # If a fall was detected at a point index greater than <check.start>, add 1 to <tank> .
  	else if (slope.indicator[i] >= 5 & i > check.start)
			tank = tank + 1
		else
			tank = 0

		i = i + 1
		}
		
	#  If the above loop was terminated because <tank> reached <tank.limit>, update the "curve.par" 
  #      slot to denote the first point at which tanking started (should be the last index checked minus <tank.limit>)
  #      also truncate <slope.indicator> so that it does not include the timepoints after tanking.  
  
	if (tank == tank.limit){
		input.well@curve.par$tanking.start = i - tank.limit
		slope.indicator = slope.indicator[1:i]
		if (!silent)
		  cat("...tanks at timepoint", i - tank.limit, ".\n")
		}
	else{
		input.well@curve.par$tanking.start = NA
		if (!silent)
			cat("...does not tank.\n")
    }
    
  #######################################################################################
  #  Method #2 of checking for tanking: see if OD falls below cutoff                    #   
  #   (as a proportion of max OD) without recovery (according to <tank.limit>)          #
  #######################################################################################
  #
  i = check.start
  tanking.start = NA
  while(i < length(y) & is.na(tanking.start)){
    # If the <tank.limit> next ODs beyond i do not reach the cutoff, then mark i as tanking. 
    if (all(y[i+(1:tank.limit)] < max(y,na.rm=T)*tank.cutoff, na.rm=T))   
      tanking.start = i
    i = i+1
  }
 
  # Graphic representation: draw the indicators used in Method #1 using the pch symbols in <slope.indicator> 
    # slope index = 2: an upward-pointing traingle for an upward jump 
    # slope index = 5: a diamond for a fall
    # slope index = 6: a downward-pointing triangle for downward jump     
	if (draw){
    points(data.from(input.well, remove = F, remove.tanking = F)[which(slope.indicator != 0),], 
    	col = 2, bg = 2, cex = 1.3, pch = slope.indicator[which(slope.indicator != 0)])
    }
    
  #######################################################################################
  #              Decide what to do about any remaining jumps in OD                      #
  #######################################################################################
    
	jump.up = which(slope.indicator == 2)
	jump.down = which(slope.indicator == 6)
	# <jump.all> is a variable which keeps track of all the jumps, whether up or down. 
	jump.all = sort(c(match(jump.down, indices), match(jump.up, indices)))
 # commented out; jump not working
 # if (length(jump.all) > 0)
    # add.info = paste("Jump(s) detected at timepoint(s)",paste(indices[jump.all],collapse=" "))
 # else
    add.info = ""
    
  # If <remove.jumps> is true, use the following automated process to try and remove OD jumps by selectively removing points from analysis.
  # if not, just return the well with the above slot filled. 
  
  if (!remove.jumps)
    input.well@add.info = add.info
  else{
    # Cycle through first few jumps (before <check.start>). <remove.initial.jump> is a logical that controls this loop.  
  	remove.initial.jump = T
  	while (length(jump.all) > 0 & jump.all[1] < check.start & remove.initial.jump){
  	
    # If any other jumps are also before <check.start>...
  		if (any(jump.all[-1] < check.start)){         
  		  # ...and the next jump is in a different direction, stop the loop and don't remove the first one. 
  			if(slope.indicator[indices[min(jump.all[-1])]] != slope.indicator[indices[jump.all[1]]])
  				remove.initial.jump = F
 				# ...or if the next two jumps are different, stop the loop and don't remove the first one. 
  			else if(length(jump.all[-1]) > 1 &
  				slope.indicator[indices[jump.all[2]]] != slope.indicator[indices[jump.all[3]]])
  				remove.initial.jump = F	
  			# ...otherwise, remove the jump and keep looping. 	
  			else 
  				remove.initial.jump = T
  			}
     # If no jumps other than the first one are before <check.start>, remove it and keep looping. 
  		else      
  			remove.initial.jump = T
  			
  	 # If the initial jump is to be removed, remove all points before the jump from analysis. 
     #  also delete the initial jump from <jump.all>  		
  		if (remove.initial.jump){
  			input.well = remove.points(input.well, 1:(indices[jump.all[1]] - 1))
  			input.well@add.info = paste(add.info, "and removed.")
  			jump.all = jump.all[-1]
  			}
  		}	
    # If greater than 3 jumps remain, discard the curve as uninterpretable	
  	if (length(jump.all) >= 4){
  		input.well = remove.points(input.well, 1:length(input.well))
  		input.well@add.info = paste(add.info, " - data was discarded.")
  		}	
  	else{
  	# If there are 3 jumps, remove all points after the last one from analysis and delete the last jump from <jump.all>
  		if(length(jump.all) == 3){
  			input.well = remove.points(input.well, indices[jump.all[3]]:length(input.well))
  			input.well@add.info = paste(add.info, "and removed.")
  			jump.all = jump.all[-3] 
  			}
  			
		# If there are now 2 jumps...
  		if(length(jump.all) == 2){
  		  # ...and they are different (one up, one down), remove the points in between them from analysis.
  			if (diff(slope.indicator[indices[jump.all]]) != 0 ){
  		 		input.well = remove.points(input.well, indices[jump.all[1]:(jump.all[2] - 1)])
  				input.well@add.info = paste(add.info, "and removed.")
  				}
 				# ...and they are in the same direction, remove all the points after the first one from analysis.
  			else{
  				input.well = remove.points(input.well, indices[jump.all[1]]:length(input.well))
	  			input.well@add.info = paste(add.info, "and removed.")
  				}				
  			}
		# If there is only one jump, remove all points after it from analysis. 
  		else if (length(jump.all == 1)){
  			input.well = remove.points(input.well, indices[jump.all[1]]:length(input.well))
  			input.well@add.info = paste(add.info, "and removed.")
  			jump.all = jump.all[-1] 
  			}
  		}
  	}
 	if(!silent)
 	  cat("\t", input.well@add.info)
	return(input.well)
	}
	
########################################################################
########################################################################
#    Check wells for growth, remove from analysis if OD is too low                 
#
#  @details
#   The well will be tagged with no.growth = T in the slot "curve.par" if raw OD values (except for <points.to.remove>)
#   do not increase beyond <growth.cutoff> above the specified time of inoculation for that well (<start.index>) 
#
check.growth = function(input.well, growth.cutoff, start.index = 2){
  
  #  Debug
  #if (all(getPosition(input.well) == c("022510b.GLBRC200DH.2","D","5"))) browser()

  # Get raw ODs (not including <points.to.remove>) and slope estimates from the well 
  # as well as OD at inoculation timepoint <start.index>
  well.pos = getPosition(input.well)
  
  # Debug 
  #if (well.pos[[2]] == "A" && well.pos[[3]] == 1)
  #  browser()
  
	raw.ODs = raw.data(input.well)[,2]
  
  # Check for missing point.
  if (any(is.na(raw.ODs)))
    exception("Warning: ", "Missing data point detected. Please check the file and try again.")
  
  start.OD = raw.ODs[start.index]
  
  raw.ODs[input.well@screen.data$Remove] = NA
	slope.estimates = slopes(input.well, remove.tanking = T, na.rm = T)

  # If fewer than 3 points remain in the analysis with valid slope estimates, discard the well. 
	if (length(slope.estimates) < 3 | all(is.na(slope.estimates)))
		input.well@curve.par$no.growth= T		
	else{
	# If there are no points at all in the raw ODs 
		if(all(is.na(raw.ODs)))
			input.well@curve.par$no.growth = T
		else if(max(raw.ODs, na.rm=T) - start.OD < growth.cutoff)   # See if OD increases by at least <growth.cutoff> above
			input.well@curve.par$no.growth = T
		else
			input.well@curve.par$no.growth = F
		}
  if(all(raw.data(input.well)[,2] - raw.data(input.well)[start.index,2] < growth.cutoff))    
    input.well@add.info = "" # This is simply to reduce the amount of unnecessary info in the output. 
                              # If the well is below growth cutoff anyway, don't bother reporting other errors. 
	return(input.well)
	}


