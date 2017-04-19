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

require(pheatmap)
require(gplots)

########################################################################
#                                                                      #
# Graphic output functions for fitted well objects. The functions are  #
#   fairly complicated and intertwined and may need revision.          #
#                                                                      #
########################################################################

# S3 Generic.
#plot <- function(input.well, ...) {
#  UseMethod("plot")
#}

########################################################################
#        Basic function plots time vs. OD from a well object           #
########################################################################
#' plot_data
#'
#' Basic function plots time vs. OD from a well object
#'
#' @param input.well The well object that need to be plottedd
#' @param unlog should data be plotted on a linear (vs. logarithmic) scale?
#' @param view.raw.data should the raw data be plotted? (
#' @param number.points should points be labeled with numeric indices?
#' @param scale determines the font scale for the entire graph. all cex values are calculated from this.
#' @param draw.symbols - should <check.slopes> be called on the well and markings drawn on the graph?
#' @param ... additional arguments passed to plot()
#' @param main ...
#' @param constant.added Similar to added.constant.
#' @param ylim ...
plot_data = function(input.well, view.raw.data = F, unlog = F, scale = 1, 
                     main = paste(plate.name(input.well), well.name(input.well)), number.points = T, 
                     draw.symbols = F, constant.added, ylim, ...){
  
  # Get data as well as a vector showing which points were removed.  
  input.data = data.from(input.well, remove = F, remove.tanking = F, raw.data=view.raw.data)
  removed.points = !(rownames(input.data) %in% rownames(data.from(input.well, remove = T, remove.tanking = T)))
  point.colors = as.character(factor(removed.points,levels=c(F,T),labels=c("black","gray80")))
  
  # Draw the axes and all text labels first.
  par(mar = c(5, 4, 4, 5)+0.1)
  plot(input.data, main = main, xlab = "Time(hours)", ylab = "log(OD - blank + const)",
       mex = scale, cex.main = 1.5*scale, cex.axis = 1.2*scale, cex.lab = 1.2*scale, 
       type ="n", ylim=ylim, ...)
  
  # Draw a second vertical axis, showing unlogged OD scale
  #  - Determine the range of the labels: from min.OD to max.OD
  if (class(try(ylim,silent=T)) == "try-error") {
    OD = unlog(input.data[,2],constant.added)
    baseline.OD = unlog(baseline(input.well),constant.added)
    min.OD = min(min(OD,na.rm=T),baseline.OD,na.rm=T)
    plateau.OD = unlog(plateau(input.well),constant.added)
    max.OD = max(max(OD,na.rm=T),plateau.OD,na.rm=T)
  } else {
    min.OD = unlog(ylim[1],constant.added)
    max.OD = unlog(ylim[2],constant.added)
  }
  #  - Compute labels and their positions
  OD.labels = seq(from = min.OD, to = max.OD, length.out = 5)
  OD.labels = signif(OD.labels,2)
  OD.at = log(OD.labels+constant.added)
  #  - Draw the axis
  axis(side=4, at=OD.at, labels=OD.labels, cex.axis = 1.2*scale, cex.lab = 1.2*scale)
  mtext(4, text = "OD - blank", line = 3, cex=1.2)
  
  # If <number.points> is true, then label each point with the index of its timepoint and plot removed points in grey, others in black. 
  if (number.points)
    text(input.data$Time, input.data[,2], rownames(input.data), col = point.colors, cex = 0.5*scale)
  # Otherwise plot all points, using a different plotting character for removed points. 	
  else
    points(input.data$Time, input.data[,2], pch = 1 + removed.points*15)
  
  # If <check.slopes> is set to T, then draw all the markings that <check.slopes> makes to determine curve parameters. 
  if (draw.symbols & !view.raw.data)
    check.slopes(input.well, draw = T)
  return()
}

########################################################################
########################################################################
#    Plot the fitted model curve from a well object if it exists      
#
# @details
#   \strong{time:} specify which points (in units of time) to plot fitted OD values for. if not specifies, plot all timepoints in range of well. 
#
plot_model = function(input.well, col = 1, scale = 1, lty = 1, time = NULL, unlog = F, constant.added=1, ...){
  
  #input.data = data.from(input.well)
  #growth = input.data[,2]
  
  # If no list of timepoints is specified, get a list of 360 timepoints (should be smooth enough) from the well's range. 
  if (is.null(time)){
    time.fin = max(data.from(input.well, raw.data = T, remove = F, remove.tanking = F)$Time)
    time = seq(0, time.fin, length.out = 360)
  }
  
  # Evaluate the predicted OD at the specified timepoints based on the fitted model. 
  predicted.OD = well.eval(input.well, time) 
  # If any values were returned, plot the lines on the current graph. Otherwise, just return without doing anything.
  if (is.numeric(predicted.OD))
    lines(time, predicted.OD, col = col, lty = lty, lw = 2 * scale) 
  else
    return()	
  
}

########################################################################
########################################################################
#   Put various parameters and info in text form on the graphs         #
#			
draw.text = function(input.well, scale = 0.5, xlim = 0, ylim = 0, auc.start = NULL, auc.end = NULL, ...){
  
  #input.data = data.from(input.well, remove = F, remove.tanking = F)
  #fit = input.well@fit.par
  
  # <text2> - fit information (fit status, model name if available, jump detection output, fit parameters if available) from well 
  #   color = red if no fit, blue if fit, green if skipped
  # <text1> - empty or inoculated well.
  #   color = green if empty, blue if inoculated, red if inoculated but has no growth or empty but has growth. 
  
  col2 = "blue" 
  text2 = paste(input.well@fit.info, input.well@model.name, input.well@add.info, "\n", parameter.text(input.well))
  
  if (length(input.well@fit.par) == 0) # no fit
    col2 = "red"
  
  if (is.empty(input.well)){
    text1 = "empty well"
    if(!lacks.growth(input.well) | length(input.well@fit.par) == 0) # growth curve fit for an empty well
      col1 = "red"
    else
      col1 = "forestgreen"
    if (length(input.well@model.name) == 0) # well was skipped 
      col1 = col2 = "forestgreen"
  }
  else{
    text1 = "inoculated well"
    if(lacks.growth(input.well) | length(input.well@fit.par) == 0) # failure to fit an inoculated well
      col1 = "red"
    else
      col1 = "forestgreen"
  }
  
  # <text1.5> - goodness of fit metric. 
  #    color = red if below 2, yellow if between 2 and 2.72, and green if above 2.72. 
  
  if(!is.na(model.good.fit(input.well))){
    #if (model.good.fit(input.well, unlog = F) > 2.72)
    #	col1.5 = "forestgreen"
    #else if (model.good.fit(input.well, unlog = F)> 2.0)
    #	col1.5 = "gold2"
    #else
    #	col1.5 = "red"
    col1.5 = "forestgreen"
    text1.5 = paste("R squared:", round(model.good.fit(input.well),3), 
                    "; AUC:", auc(input.well, start = auc.start, end = auc.end), 
                    "; AUC.OD:", auc.OD(input.well, start = auc.start, end = auc.end))
  }
  else
    col1.5 = text1.5 = NULL
  
  # Print all text at the top of the graph with approprate positions and scaling  	
  text(x = xlim[1] + 0.50 * diff(xlim), y = ylim[2] - 0.025 * diff(ylim), 
       text1.5, cex = 1.5*scale, col = col1.5) 
  
  text(x = xlim[1] + 0.50 * diff(xlim), y = ylim[2] - 0 * diff(ylim), 
       text1, cex = 1.5*scale, col = col1)
  
  text(x = xlim[1] + 0.50 * diff(xlim), y = ylim[2] - 0.03 * diff(ylim), 
       text2, pos = 1, cex = 1.5*scale, col = col2)	
}	

########################################################################
########################################################################
#        Draw lines on graph denoting calculated parameters          
#
#  @details	
# \strong{show.num} - should curve parameters be labeled? 
#		
draw.calc.par = function(input.well, scale = 0.5, unlog = F, constant.added, show.num = T){
  
  # Don't do anything if well was not fit. 
  if (is.null(well.eval(input.well)))
    return()
  
  # Collect values for various curve parameters. 
  baseline = baseline(input.well)
  inoc.log.OD = inoc.log.OD(input.well)
  max.log.OD = max.log.OD(input.well) 
  plateau = plateau(input.well) 
  inflection.time = input.well@inflection.time # was a param in model
  fin.time = (inflection.time+max(data.from(input.well)[,1]))/2
  
  # <inflection.time> = timepoint at greatest growth 
  # <max.y> = OD measurement at <inflection.time>, minus the constant added before taking the log (if reversing the transformation)
  # <max.slope> = slope (on log scale) at <inflection.time> (specific growth)
  # had to add the unlog code. was calculated differently before NWD 7/21/14
  max.slope = max.spec.growth.rate(input.well)
  max.y = well.eval(input.well, inflection.time)
  lag.x = lag.time(input.well) 
  lag.y = baseline
  
  # ---- Specific growth rate ---- #
  lines(c(lag.x, inflection.time), c(lag.y, max.y), lty = 2, col = "red")
  
  
  # Blue dotted line at time of maximum growth, with text label for specific growth rate. 
  abline(v = inflection.time, lty = 2, lw = (scale^2)*2, col = "blue")
  if(show.num) text(inflection.time, max.y, round(max.slope,3), col = "blue", cex = 1.5*scale, pos = 2)
  
  #  inoculation OD and baseline of the fitted model
  abline(h = inoc.log.OD, lw = scale*2, lty = 3)
  abline(h = baseline, col = "red", lw = (scale^2)*2, lty = 2)
  if(show.num) {
    text(fin.time, inoc.log.OD, paste(round(inoc.log.OD,3),"\n",sep="") , col = "black", cex = 1.5*scale, pos = 2)
    text(fin.time, baseline, paste("\n\n", round(baseline,3), sep="") , col = "red", cex = 1.5*scale, pos = 2)
  }
  
  # ---- Lag time ---- #
  # Do not draw a horizontal line to lag time if it is 0 or negative. 
  # Otherwise draw a red line from the starting point to the lag time, and label with the lag time 
  if (lag.time(input.well) == 0){
    if(show.num) text(0,  inoc.log.OD, "\n\n0.000", col = "red", cex = 1.5*scale, pos = 4)
  }
  else{
    lines(c(0, lag.x), c(baseline, baseline), col = "red", lw = (scale^2)*2, lty = 2)
    if(show.num) text(lag.x, lag.y, paste("\n\n", round(lag.time(input.well),3)), col = "red", cex = 1.5*scale, pos = 2)
  }
  
  # ---- Total growth ---- #
  
  # Draw horizontal lines for the max.log.OD in black, the plateau in green and the initial OD in black.
  abline(h = max.log.OD, lty = 3, lw = scale*2)
  abline(h = plateau, lty = 2, lw = (scale^2)*2, col = "forestgreen")
  
  # Draw a vertical line from the initial OD to the final OD in black, and then to the plateau in gray. 
  lines(c(fin.time, fin.time), c(inoc.log.OD, max.log.OD), lw = (scale^2)*2, lty = 3)
  lines(c(fin.time, fin.time), c(max.log.OD, plateau), lw = (scale^2)*2, lty = 3, col = "grey")
  
  # Text: plateau and initial ODs (on left), difference between initial and final OD on right
  if(show.num){
    text(fin.time, plateau, paste(round(plateau,3),"\n",sep="") , col = "forestgreen", cex = 1.5*scale, pos = 2)
    text(fin.time, max.log.OD, paste("\n\n\n",round(max.log.OD,3),sep="") , col = "black", cex = 1.5*scale, pos = 2)
    text(fin.time, .5*(max.log.OD-inoc.log.OD)+inoc.log.OD, round(max.log.OD - inoc.log.OD,3), cex = 1.5*scale, pos = 4)
    #       difference between final and plateau OD (if large enough)  
    if (!reach.plateau(input.well))
      text(fin.time, .5*(plateau-max.log.OD)+max.log.OD, paste("(", round(plateau - max.log.OD,3), ")", sep = ""), col = "grey", cex = 1.5*scale, pos = 2) 
  }
}

########################################################################
########################################################################
#  Draw residuals from the nonlinear fit with option for lowess line   #
#		
plot_residuals = function(input.well, xlim = NULL, lowess = T, ...){
  well = input.well
  data = data.from(well, remove = F, remove.tanking = F)
  
  if (is.null(xlim))
    xlim = c(min(data$Time, 0)-1, max(data$Time))
  
  plot(data.from(well)[,1], model.residuals(well), main = paste(plate.name(well), well.name(well), "\n[Residuals]"),
       xlab = "Time(hours)", ylab = paste("Residual", names(data)[2]), xlim = xlim)
  
  abline(0,0, lty = 2)
  
  if (lowess)
    lines(lowess(data.from(well)[,1], model.residuals(well)), lw = 2, col = "red")
}

##############################################################################
##############################################################################
#' Create a heat map of a plate
#' 
#' @details
#' This function is used to create a heatmap using 
#' specific growth, total growth, or lag time
#' for each well on a plate.
#'
#' @param fitted.well.array matrix containing well array object data
#' @param attribute the data type we should use to create a heatmap
#' @param MinMax The specific range for the heatmap. 
#' @param constant.added the numeric constant that was added to each curve before the log transform: 
#' same as \code{add.constant} in \link{gcat.analysis.main}
#'
#' @return path of heatmap pdf file
#' 
create.heatmap = function(fitted.well.array, attribute, MinMax = NA, constant.added){
  # debug
  #browser()
  
  attr.name <- deparse(substitute(attribute))
  pdf.name <- ""
  if(class(fitted.well.array) == "matrix"){
    #We may want to sub() out periods from plate.ID if it causes problems
    plate.ID = unique(unlist(aapply(fitted.well.array,plate.name)))[1]
    if (attr.name == "achieved.growth.OD") {
      #  Need to pass constant.added to the attribute computation function
      spec.growth = unlist(aapply(fitted.well.array, attribute, constant.added = constant.added))
    } else {
      #  The attribute computation function does not have a constant.added argument
      spec.growth = unlist(aapply(fitted.well.array, attribute))
    }
    
    num.dig = 3 #how many digits should be put on pdf?
    max = round(max(spec.growth, na.rm=T), digits=num.dig)
    min = round(min(spec.growth, na.rm=T), digits=num.dig)
    min_spec.growth = min
    max_spec.growth = max
    
    avg = round(median(spec.growth, na.rm=T), digits=num.dig)
    
    attr.name <- sub("\\.", "_", attr.name) #do not want periods in file path
    letters <- attr(fitted.well.array, "dimnames")[[1]]
    for(i in 1:length(letters)) letters[i] = paste(" ", letters[i], " ")
    nums <- attr(fitted.well.array, "dimnames")[[2]]
    for(i in 1:length(nums)) nums[i] = paste(" ", nums[i], " ")
    heat <- matrix(spec.growth, nrow=dim(fitted.well.array)[1], ncol=dim(fitted.well.array)[2], dimnames=list(letters,nums))
    pdf.name <- paste(getwd(), "/", plate.ID, "_", attr.name, ".pdf", sep="")
    
    #heatmap(heat, Rowv=NA, Colv=NA, revC=T, scale="none", na.rm=T, main=plate.ID, col=rainbow(100), margins=c(6,6))
    #mtext(paste("Max:", round(max(spec.growth, na.rm=T), digits=4),"Min:", round(min(spec.growth, na.rm=T), digits=4), "Avg:", round(mean(spec.growth, na.rm=T), digits=4)), side=1, line=3)
    if (length(MinMax) == 2){
      #  Min and max values to plot to floor and ceiling values if MinMax parameter was specified by the caller
      if(!(is.na(MinMax[1]))) {
        if(MinMax[1] > max_spec.growth) {
          exception("", paste("Heat map range specified by user does not overlap with the actual range of ", attr.name, ". The actual range is from ", max_spec.growth, " to ", min_spec.growth))
        }
        else {
          heat[is.finite(heat) & heat < MinMax[1]] = MinMax[1]
          #  Change min to minimum value specified by user
          min = MinMax[1]  
        }
      }
      if(!(is.na(MinMax[2]))) {
        if(MinMax[2] < min_spec.growth) {
          exception("", paste("Heat map range specified by user does not overlap with the actual range of ", attr.name, ". The actual range is from ", max_spec.growth, " to ", min_spec.growth))
        }
        else {
          heat[is.finite(heat) & heat > MinMax[2]] = MinMax[2]
          #  Change max to maximum value specified by user
          max = MinMax[2]
        }
      }
    }
    
    heat.text = paste(toupper(sub("\\.", " ", attr.name)), ":\n", plate.ID, "\n",
                      paste("Min:", min, "Med:", avg, "Max:", max, sep=" "))
    
    # Fix to Issue5. Before calling pdf(pdf.name) check if pheatmap is possible to plot for the data. If not 
    #       then print the error message in the output pdf file
    
    if (class(try(pheatmap(heat, color=colorpanel(100, "red", "orange", "yellow"),
             border_color="black", cell_width=2, cell_height=3,
             cluster_rows=F, cluster_cols=F, scale='none', main=heat.text, fontsize=16))) == "try-error") {
      pdf(pdf.name)
      plot(1, type="n", axes=F, xlab="", ylab="", main = paste(heat.text, "\n", paste("Not Enough Values to plot a heatmap")))
      dev.off()
    }
    
    else {
      pdf(pdf.name)
      pheatmap(heat, color=colorpanel(100, "red", "orange", "yellow"),
               border_color="black", cell_width=2, cell_height=3,
               cluster_rows=F, cluster_cols=F, scale='none', main=heat.text, fontsize=16)
      dev.off()
    }
    
  }
  else {
    return("Error") 
  }
  return(pdf.name)
}

########################################################################
########################################################################
# Plate overview graphic
# 
# @details
#  Draw grids of 96 points as a visual representation of fit status,   #
#  and other info for an array of fitted well objects, plate by plate  #
#
plate.overview = function(fitted.well.array, scale = 1, plate.ncol = 12, plate.nrow = 8){
  
  
  # Start with a list of the unique plate names in the fitted well array 
  # and an appropriately-sized grid of coordinates to plot wells on.
  plates = unique(unlist(aapply(fitted.well.array,plate.name)))
  
  grid = data.frame(x = rep(rep(1:plate.ncol, each = plate.nrow), length(plates)),
                    y = rep( rep(-(1:plate.nrow), times = plate.ncol), length(plates)))
  
  
  # Gather information on each well to display on each of the coordinates in <grid>:
  #   - was it marked as empty in the plate layout?
  #   - did the program find it to contain no growth ("dead")? 
  #   - was the fitting procedure successful? 
  #   - did the curve tank? if so, at what timepoint? if not, or if the curve was marked as dead anyway, do not display the value. 
  #   - does the "additional info" slot indicate that any points were removed or the whole well discarded?
  
  empty = unlist(aapply(fitted.well.array, is.empty))
  dead  = unlist(aapply(fitted.well.array, lacks.growth))
  fit = unlist(aapply(fitted.well.array, contains.fit))
  
  tanking = unlist(aapply(fitted.well.array, tanking.start))
  tanking[is.na(tanking) | tanking == 1 | dead] = ""
  
  errors = unlist(aapply(fitted.well.array, function(well){
    if (length(well@add.info) == 0)
      ""
    else if (grepl("removed", well@add.info))
      "-"
    else if (grepl("detected", well@add.info))
      "+"
    else if (grepl("discarded", well@add.info))
      "!"
    else
      ""
  }))
  
  # Color and plotting character vectors (length = the number of wells in the array)
  # Default = 1 (open point, black)
  colors = char = rep(1, length(tanking))
  
  # Desired colors
  colors[empty & dead] = "green3" # Empty well with no growth.
  colors[!empty & fit] = "blue" # Inoculated well with successfully fitted growth curve.
  
  # Undesired colors 
  colors[empty & !dead] = "darkolivegreen4" # Inoculated well with some growth. 
  colors[!empty & !fit] = "red" # Inoculated well with no successfully fit (either no growth or unsuccessful fit).
  
  char[!dead & fit] = 19 # Filled points for non-empty wells with successful fits 
  char[!dead & !fit] = 4 # an X for non-empty wells with failed fits. 
  
  char[errors == "!"] = 8 # Asterisk for discarded wells. 
  char[errors == "-" & dead ] = 5 # Open diamond for empty wells (after removing points).
  char[errors == "-" & !dead & fit] = 23 # Filled diamond for non-empty wells with removed points and successful fits. 
  char[errors == "-" & !dead & !fit] = 8 # Asterisk for wells with removed points and failed fits.
  
  
  for (plate in 1:length(plates)){
    
    indices = (plate - 1) * plate.nrow*plate.ncol + 1:(plate.nrow*plate.ncol)
    
    # Plot the grid using colors and plotting characters determined above. 
    plot(grid[indices,], col = colors[indices], bg = colors[indices], pch = char[indices], 
         main = plates[plate], mex = scale, cex = scale, cex.main = 1.5*scale, cex.axis = 1.2*scale, 
         xaxt = "n", yaxt = "n", xlim = c(-0.5,plate.ncol + 0.5), ylim = c(-(plate.nrow + 1.5), 0.5), xlab = "", ylab = "")
    
    # Symbol legends
    
    legend.xpos = (c(-1,2.75,6.5,6.86,10.25)+0.5)*(plate.ncol+1)/13 - 0.5
    legend.ypos = -(plate.nrow + 0.5)
    
    legend(x=legend.xpos[1], y= legend.ypos, cex = 0.7 * scale, y.intersp = 1.5, bty="n",  
           legend=c("Empty, no growth","Empty with growth"),
           pch = c(1,19),
           pt.bg = c("green3","darkolivegreen4"),
           col = c("green3","darkolivegreen4")
    )
    legend(x=legend.xpos[2], y= legend.ypos, cex = 0.7 * scale, y.intersp = 1.5, bty="n",  
           legend=c("Inoculated with growth", "Inoculated, no growth"),
           pch = c(19,1),
           pt.bg = c("blue","red"),
           col = c("blue","red")
    )
    legend(x=legend.xpos[3], y= legend.ypos, cex = 0.7 * scale, y.intersp = 1.5, bty="n",  
           legend=c("Well tanks at specified index", "Some points removed"),
           pch = c(21,23),
           pt.bg = c("grey","grey"),
           col = c("black","black")
    )  
    
    text(x=legend.xpos[4], y=legend.ypos - 0.29,"#",cex=0.5*scale)    
    
    legend(x=legend.xpos[5], y=legend.ypos, cex = 0.7 * scale, y.intersp = 1.5, bty="n",  
           legend=c("Model fitting failed", "Well discarded"),
           pch = c(4,8),
           pt.bg = c("black","black"),
           col = c("black","black")
    )
    
    # Add tanking indices if any were found. 
    text(grid[indices,] + 0.30, cex = 0.75*scale, 
         labels = tanking[indices], col = colors[indices])
    
    # Label rows and columns
    text(-1, -1:-plate.nrow, pos = 4, LETTERS[1:plate.nrow], cex = scale) 
    text( 1:plate.ncol, 0 , 1:plate.ncol, cex = scale)  	
  }
  
}

########################################################################
########################################################################
#  Draw individual fitted wells
#  
#  @details
#  Draw each well in an array of fitted well objects in succession.    #
#  Include options for adding notations, text info and fit parameters. #
#
view.fit = function(fitted.data, indices = 1:length(fitted.data), 
                    unlog = F, constant.added, xlim = NULL, ylim = NULL, display.legend = T, 
                    show.text = T, show.calc = T, draw.guess = NULL, draw.symbols = F, number.points = T, 
                    user.advance = T, show.residuals = F, scale = 1, auc.start = NULL, auc.end = NULL, ...){
  
  if(!is.array(fitted.data))
    fitted.data = list(fitted.data)
  
  # Determine the boundaries for the axes (if user did not specify them)
  if(is.null(ylim)){
    min.y = min(unlist(aapply(fitted.data, function(well){
      if (unlog) well@use.log = F
      min.y = min(data.from(well, remove = F, remove.tanking = F)[,2], na.rm = T)
      min(min.y, well@fit.par$b)
    })))
    max.y = max(unlist(aapply(fitted.data, function(well){
      if (unlog) well@use.log = F
      max.y = max(data.from(well, remove = F, remove.tanking = F)[,2], na.rm = T)
      max(max.y, well@fit.par$b + well@fit.par$A)
    })))
    ylim = c(min.y, min.y + (max.y-min.y)*1.15) - unlog*constant.added
  }
  if(is.null(xlim)){
    min.x = min(unlist(aapply(fitted.data, function(well){
      min(data.from(well, remove = F, remove.tanking = F)[,1], na.rm = T)
    })))
    max.x = max(unlist(aapply(fitted.data, function(well){
      max(data.from(well, remove = F, remove.tanking = F)[,1], na.rm = T)
    })))
    xlim = c(min.x - 0.05 * (max.x-min.x), max.x)
  }
  
  # Display a figure legend
  if(display.legend){
    well.fit.legend(xlim=xlim,ylim=ylim,scale=scale,constant.added=constant.added)
    if(user.advance){
      prompt = readline("<Enter> to continue or Q to quit >>")
      if (toupper(prompt) == "Q") break
    }
  }
  # Start to cycle through the wells 
  well.number = 1
  while (well.number <= length(fitted.data)) {		
    # Only show wells specified by <indices> (default all wells)
    if (well.number %in% indices){ 
      # plot the well
      fitted.well = fitted.data[[well.number]]
      plot(x=fitted.well, constant.added = constant.added, xlim = xlim, ylim = ylim,
           unlog = unlog, well.number = well.number, scale = scale, number.points = T, draw.symbols = F, show.text = T, 
           show.calc = T, draw.guess = NULL, auc.start = auc.start, auc.end = auc.end, ...)
      
      if(user.advance)
        cat("\n[", well.number, "] ", plate.name(fitted.well), " ", well.name(fitted.well), ".", sep = "")
      
      if (show.residuals & is.numeric(model.residuals(fitted.well))){
        if(user.advance)
          if (toupper(readline("<Enter> for residuals >>")) == "Q") break
        plot_residuals(fitted.well)
      }
      
      # Allow user to advance the currently shown well if specified. 
      if (user.advance){
        
        prompt = readline("<Enter> to continue, or type # of next well or Q to quit >>")
        if (toupper(prompt) == "Q") break
        
        user.input = suppressWarnings(try(as.numeric(prompt),silent=T))
        
        # Go onto the next well unless input is a number. 
        if (is.numeric(user.input) & !is.na(user.input) & length(user.input) > 0)
          well.number = user.input - 1
      }
    }
    # Advance the loop
    well.number = well.number + 1
  }		
}	

#  Draw legend on a well plot
well.fit.legend = function(xlim, ylim, scale = 1, constant.added){
  par(mar = c(5, 4, 4, 5)+0.1)
  plot(0,0, main = "[Index] <Plate Name> <Well Position>\n<Strain Name>; <Media Definition>",
       xlim = xlim, ylim = ylim, xlab = "Time", ylab = "log(OD - blank + const)", 
       mex = scale, cex.main = 1.5*scale, cex.axis = 1.2*scale, cex.lab = 1.2*scale, type = "n")
  
  # Draw a second vertical axis, showing unlogged OD scale
  min.OD = unlog(ylim[1],constant.added)
  max.OD = unlog(ylim[2],constant.added)
  OD.labels = seq(from = min.OD, to = max.OD, length.out = 5)
  OD.labels = round(OD.labels,1)
  OD.at = log(OD.labels+constant.added)
  axis(side=4, at=OD.at, labels=OD.labels, cex.axis = 1.2*scale, cex.lab = 1.2*scale)
  mtext(4, text = "OD - blank", line = 3, cex=1.2)
  
  # Sample max. slope line
  abline(v=min(xlim)+0.5*max(xlim), col="blue", lty=2)
  text(mean(xlim),min(ylim)+0.4*diff(ylim),labels="Maximum specific\ngrowth rate",col="blue",pos=2,cex=0.75*scale)
  
  # Sample plateau line
  abline(h=min(ylim)+0.8*diff(ylim),col="forestgreen",lty=2)
  text(min(xlim)+0.9*diff(xlim),ylim+0.8*diff(ylim),labels="Growth plateau",col="forestgreen",pos=3,cex=0.75*scale)
  
  # Sample max.log.OD line
  abline(h=min(ylim)+0.7*diff(ylim),col="black",lty=3)
  text(min(xlim)+0.9*diff(xlim),ylim+0.7*diff(ylim),labels="max.log.OD",col="black",pos=1,cex=0.75*scale)
  
  # Sample inoc.log.OD
  abline(h=min(ylim)+0.1*diff(ylim),col="black",lty=3)
  text(min(xlim)+0.1*diff(xlim),min(ylim)+0.1*diff(ylim),labels="Fitted growth\nat inoculation",col="black",pos=3,cex=0.75*scale)
  
  # Sample baseline
  abline(h=min(ylim)+0.05*diff(ylim),col="red",lty=2)
  text(min(xlim)+0.1*diff(xlim),min(ylim)+0.05*diff(ylim),labels="Baseline",col="red",pos=1,cex=0.75*scale)
  
  # Sample lag time
  lines(min(xlim)+c(0.1,0.25,0.50)*max(xlim),min(ylim)+c(0.05,0.05,0.4)*diff(ylim),col="red",lty=2)
  text(min(xlim)+0.25*max(xlim),min(ylim)+0.05*diff(ylim),labels="Lag time",col="red",pos=1,cex=0.75*scale)
  
  # Sample achieved growth
  lines(min(xlim)+c(0.75,0.75)*max(xlim),min(ylim)+c(0.1,0.7)*diff(ylim),col="black",lty=3)
  text(min(xlim)+0.75*max(xlim),min(ylim)+0.3*diff(ylim),labels="Achieved growth",col="black",cex=0.75*scale)
  
  # Sample plateau - achieved growth
  lines(min(xlim)+c(0.75,0.75)*max(xlim),min(ylim)+c(0.7,0.8)*diff(ylim),col="grey",lty=3)
  text(min(xlim)+0.75*max(xlim),min(ylim)+0.75*diff(ylim),labels="Projected minus achieved growth",col="grey",cex=0.75*scale)
  
  # Symbol legend
  legend(x="right", title = "Timepoint Symbols", legend = c("Normal point", "Ignored point"),
         cex = 0.75*scale, pt.cex = c(0.6,0.6)*scale, pch = c(35,35), col=c("black","gray80"),
         x.intersp=1, xjust = 1, y.intersp=1.5)
}

#  Generate pdf files
pdf.by.plate = function(fitted.data, out.prefix = "", upload.timestamp = NULL, 
                        out.dir = getwd(), unlog = F, constant.added, silent = T, overview.jpgs = T, plate.ncol = 12, plate.nrow = 8,
                        lagRange = NA, specRange = NA, totalRange = NA, totalODRange = NA, auc.start = NULL, 
                        auc.end = NULL, ...){
  
  # Prepare timestamp for addition to output file names. 
  filename.timestamp = strftime(upload.timestamp, format="_%Y-%m-%d_%H.%M.%S")
  
  # Start file list with the overview pdf
  file.list.out = paste(out.dir,"/",out.prefix, "_overview", filename.timestamp, ".pdf",sep="")
  
  # Call <plate.overview> to draw a graphic representation of each plate in this file. 
  pdf(file.list.out, title = paste(out.prefix, "plate overview"))
  plate.overview.out = try(plate.overview(fitted.data),silent=T)
  if(class(plate.overview.out) == "try-error")
    stop("Error in <plate.overview>: ", plate.overview.out)
  
  # Close devices
  while(dev.cur() != 1)
    dev.off() 
  
  # Cycle through each plate 
  for(i in 1:dim(fitted.data)[3]){
    
    # Get plate ID and position in data array.
    plate.ID = dimnames(fitted.data)[[3]][i]
    plate.indices = (i-1) * plate.nrow*plate.ncol + 1:(plate.nrow*plate.ncol)
    if(overview.jpgs){
      # most be > 1 to partition value breaks for heatmap
      well.matrix <- aapply(fitted.data[,,i], max.spec.growth.rate) 
      num.wells <- length(well.matrix[!sapply(well.matrix, is.na)])
      if(num.wells > 1){
        #Heatmap block##########################################################
        #alongside the jpgs file create 3 heatmaps for each plate. NWD
        spec.heat.file = try(create.heatmap(fitted.data[,,i], max.spec.growth.rate, MinMax = specRange, constant.added))
        if(class(spec.heat.file) == "try-error") {
          if(!silent)
            exception("", paste("Error in <create.heatmap> for specific growth ", paste(trimws(spec.heat.file), "for specRange parameter")))  
          else
            exception("", paste(trimws(spec.heat.file), "for specRange"))
        }
        lag.heat.file = try(create.heatmap(fitted.data[,,i], lag.time, MinMax = lagRange, constant.added))
        if(class(lag.heat.file) == "try-error") {
          if(!silent)
            exception("", paste("Error in <create.heatmap> for lag time ", paste(trimws(lag.heat.file), "for lagRange parameter")))
          else
            exception("", paste(trimws(lag.heat.file), "for lagRange"))
        }
        total.heat.file = try(create.heatmap(fitted.data[,,i], achieved.growth, MinMax = totalRange, constant.added))
        if(class(total.heat.file) == "try-error") {
          if(!silent)
            exception("", paste("Error in <create.heatmap> for total growth ", paste(trimws(total.heat.file), "for totalRange paramter")))
          else
            exception("", paste(trimws(total.heat.file), "for totalRange"))
        }
        total.OD.heat.file = try(create.heatmap(fitted.data[,,i], achieved.growth.OD, MinMax = totalODRange, constant.added))
        if(class(total.OD.heat.file) == "try-error") {
          if(!silent)
            exception("", paste("Error in <create.heatmap> for total OD growth ", paste(trimws(total.OD.heat.file), "for totalODRange parameter")))
          else
            exception("", paste(trimws(total.OD.heat.file), "for totalODRange"))
        }
          
        #  Add name of file if successfully written to file list output. Including heatmap files NWD
        file.list.out = c(file.list.out, spec.heat.file, lag.heat.file, total.heat.file, total.OD.heat.file)
        ########################################################################
      }
      jpg.name = paste(out.dir, "/", plate.ID, "_overview", ".jpg", sep="")
      jpeg(jpg.name, quality = 90, width = 600, height = 480)
      plate.overview.out = try(plate.overview(fitted.data[,,i]),silent = T)
      if(class(plate.overview.out) == "try-error")
        stop("Error in <plate.overview>: ", plate.overview.out)
    }
    else
      jpg.name = c()
    
    # Open a separate PDF for each plate.
    if(!silent) cat("\nprinting PDF for", plate.ID)
    pdf.name = paste(out.dir, "/", plate.ID, "_plots", filename.timestamp, ".pdf", sep="")
    pdf(pdf.name, title = paste("R Graphics output for plate", plate.ID))
    
    # Call <view.fit> to draw each well on the plate to the pdf. 
    view.fit.out = try(view.fit(fitted.data, indices = plate.indices, unlog=unlog, constant.added=constant.added, 
                                user.advance=F, auc.start = auc.start, auc.end = auc.end, ...),silent=T) 
    
    if(class(view.fit.out) == "try-error")
      stop("Error in <view.fit>: ", view.fit.out)
    
    # Close all devices
    while(dev.cur() != 1)
      dev.off() 
    
    if(!silent) cat("...done!\n\twritten to", pdf.name, "\n")  
    file.list.out = c(file.list.out, jpg.name , pdf.name)
  }
  return(file.list.out)
} 