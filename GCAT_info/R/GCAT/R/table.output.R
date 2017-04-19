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
#' Populate an output table with parameters and other useful info for each well in a fitted dataset.
#'
#' @param fitted.data.set array of fitted well objects
#' @param unlog - Should OD values be returned on the linear scale instead of log-transformed scale? 
#' @param constant.added - For returning values on linear scale, what constant was added to ODs before the log transform? 
#' @param reach.cutoff - what proportion of the plateau OD must tbe reached by the last valid timepoint for the curve to be marked as reaching 
#' its plateau OD?
#' @param filename.timestamp timestamp for addition to output file names (for file references in last column of the output)
#' @param use.linear.param did the model formula contain a linear parameter?  Should normally be FALSE, as the linear parameter is deprecated
#' @param use.loess was LOESS used to fit the data (instead of unsing a growth curve model formula)?
#' @param auc.start Start time for AUC computation
#' @param auc.end End time for AUC computation
#'
table.out = function(fitted.data.set, unlog = F, constant.added, reach.cutoff = 0.90, 
                     filename.timestamp = NULL, use.linear.param=F, use.loess=F,
                     auc.start = NULL, auc.end = NULL){
  
  # The idea is basically to use <unlist> and <aapply> on the fitted data array in order 
  # to get one vector for each column of the output table.  
  
  # Get identifying information (plate, well, media and strain names)
  plate.ID = unlist(aapply(fitted.data.set,plate.name))
  well.ID = unlist(aapply(fitted.data.set,well.name))
  media.ID = unlist(aapply(fitted.data.set,media.name))
  strain.ID = unlist(aapply(fitted.data.set,strain.name))
  # Get fit information for each well
  #   - was it marked as empty in the plate layout?
  #   - did the program find it to contain no growth ("dead")? 
  #   - was the fitting procedure successful? 
  #   - did the curve tank? if so, at what timepoint? if not, set value to "-"
  
  empty = unlist(aapply(fitted.data.set, is.empty))
  dead  = unlist(aapply(fitted.data.set, lacks.growth))
  fit = unlist(aapply(fitted.data.set, contains.fit))
  tanking = unlist(aapply(fitted.data.set, tanking.start))
  tanking[is.na(tanking) | tanking == 1 | dead] = "-"
  
  # Get calculated values for each well: specific growth, final and initial OD, fitted plateau and baseline OD, lag time, etc.
  inflection.time = unlist(aapply(fitted.data.set, inflection.time))
  max.spec.growth.rate = unlist(aapply(fitted.data.set, max.spec.growth.rate))
  max.log.OD = unlist(aapply(fitted.data.set, max.log.OD))
  inoc.log.OD = unlist(aapply(fitted.data.set, inoc.log.OD))
  projected.growth = unlist(aapply(fitted.data.set, projected.growth))
  projected.growth.OD = unlist(aapply(fitted.data.set, projected.growth.OD, constant.added))
  achieved.growth = unlist(aapply(fitted.data.set, achieved.growth))
  achieved.growth.OD = unlist(aapply(fitted.data.set, achieved.growth.OD, constant.added))
  auc = unlist(aapply(fitted.data.set, auc, start=auc.start, end=auc.end))
  auc.OD = unlist(aapply(fitted.data.set, auc.OD, start=auc.start, end=auc.end, constant.added=constant.added))
  lag.time = unlist(aapply(fitted.data.set, lag.time))
  shape.par = unlist(aapply(fitted.data.set, shape.par))
  RSS = unlist(aapply(fitted.data.set, rss))
  baseline = unlist(aapply(fitted.data.set, baseline))
  amplitude = unlist(aapply(fitted.data.set, amplitude))
  plateau = unlist(aapply(fitted.data.set, plateau))
  ########################3h#############################################
  max.spec.growth.rate.SE = unlist(aapply(fitted.data.set, max.spec.growth.rate.SE))
  shape.par.SE = unlist(aapply(fitted.data.set, shape.par.SE))
  lag.time.SE = unlist(aapply(fitted.data.set, lag.time.SE))
  amplitude.SE = unlist(aapply(fitted.data.set, amplitude.SE)) # a.k.a amplitude error
  baseline.SE = unlist(aapply(fitted.data.set, baseline.SE)) # a.k.a baseline error
  #######################################################################
  
  # If the curve falls short of 90% of plateau OD by the final timepoint.
  no.reach.plateau = !unlist(aapply(fitted.data.set, reach.plateau, cutoff = 0.9))
  # If the fitted baseline is below zero on linear scale
  no.reach.baseline = unlog(baseline,constant.added) < 0
  
  # If any of these are NA as a result of failed fits, change them to false: they don't need to be reported.  
  no.reach.plateau[is.na(no.reach.plateau)] = F
  no.reach.baseline[is.na(no.reach.baseline)] = F
  # What percent of the total growth does the curve actually reach? 
  # (in case of total growth being 0, change this to 100%)
  percent.reach = 100*((max.log.OD - inoc.log.OD) / (projected.growth))
  percent.reach[is.infinite(percent.reach)] = 100
  
  # Return the name of the model (if any) that was successfully fit to the well. 
  model.used = unlist(aapply(fitted.data.set, function(well)well@model.name))
  
  # "Goodness of fit" metric
  good.fit = unlist(aapply(fitted.data.set, model.good.fit))
  
  # Code the two flags: 
  flag1 = flag2 = rep("-", length(tanking))
  
  for(i in 1:length(tanking)){	
    #  Flag 1 (empty/inoculated flag) possible values:
    #   well was empty and no growth was found (E)
    #   well was empty, but growth was found (E*)
    #   well was inoculated but no growth was found (!)
    #   well was inoculated and growth was found (I)
    
    if(empty[i] & !fit[i])
      flag1[i] = "E "
    if(empty[i] & fit[i])
      flag1[i] = "E*"
    if(!empty[i] & dead[i])
      flag1[i] = "! "
    if(!empty[i] & !dead[i])
      flag1[i] = "I "
    
    #  Flag 2 (lower/upper asymptotes) possible values:
    #   well did not reach lower asymptote (baseline OD) (L)
    #   well did not reach upper asymptote (plateau OD) (U)
    #   well did not reach either asymptote (L/U)
    #   well reached both asymptotes (-)
    
    if(no.reach.baseline[i]){
      if (no.reach.plateau[i])
        flag2[i] = "L/U"
      else
        flag2[i] = "L"
    }
    else{
      if (no.reach.plateau[i])
        flag2[i] = "U"
      else
        flag2[i] = "-"
    }
    # Also use the <dead> and <empty> and <fit> to provie more info about why model fitting failed in some cases. 
    if(dead[i])
      model.used[i] = "<NA>: skipped"
    else if(!empty[i] & !fit[i])
      model.used[i] = "<NA>: failed"	
  }
  
  # Flag 3: return the additional info slot. 
  flag3 = unlist(aapply(fitted.data.set, function(well){
    if (length(well@add.info) > 0) 
      return(well@add.info)
    else
      return("")
  }))
  
  # If something is amiss with the data table use this to check on the arguments...
  #cat("plate ", length(plate.ID)," well ", length(well.ID)," media ", length(media.ID)," strain ", length(strain.ID),
  #" model ", length(model.used)," max.spec.growth.rate", length(max.spec.growth.rate), "projected.growth", length(projected.growth),
  #"lag.time", length(lag.time), "inoc.log.OD", length(inoc.log.OD), "good.fit",
  #length(good.fit),"empty", length(flag1),"asymp", length(flag2)," tank ", length(tanking)," reach ", length(percent.reach)," other ", length(flag3), sep = "\n")
  # 06.28.11: Add a row number identifier for output perusal
  row.number = 1:length(plate.ID)
  
  pdf.file = page.no = c()
  # 06.29.11: Add pdf file name and page number references. Prepare timestamp for addition to output file names (for file references in last column)
  for(i in 1:length(plate.ID)){
    pdf.file[i] = paste(plate.ID[i], "_plots", filename.timestamp, ".pdf", sep="")
    page.no[i] = (i-1) %% 96 + 2
  }
  # Slap it all together into a data frame.
  if(use.loess){
    output.core = data.frame(row = row.number, plate = plate.ID, well = well.ID, media = media.ID, strain = strain.ID, 
                             model = model.used, lag.time, inflection.time, max.spec.growth.rate, 
                             baseline, amplitude, plateau, inoc.log.OD, max.log.OD, achieved.growth, AUC=auc,
                             baseline.OD = unlog(baseline,constant.added), amplitude.OD = unlog(amplitude,constant.added), 
                             plateau.OD = unlog(plateau,constant.added), inoc.OD = unlog(inoc.log.OD,constant.added), 
                             max.OD = unlog(max.log.OD,constant.added), achieved.growth.OD = achieved.growth.OD, 
                             AUC.OD = auc.OD,
                             R.squared = good.fit, RSS = RSS, empty = flag1, asymp.not.reached = flag2, tank = tanking, other = flag3, pdf.file = pdf.file, page.no = page.no)
  } else {
    output.core = data.frame(row = row.number, plate = plate.ID, well = well.ID, media = media.ID, strain = strain.ID, 
                             model = model.used, lag.time = lag.time, lag.time.SE, inflection.time, max.spec.growth.rate, max.spec.growth.rate.SE, 
                             baseline, baseline.SE, amplitude, amplitude.SE, plateau, inoc.log.OD, max.log.OD, projected.growth, achieved.growth, AUC=auc,
                             baseline.OD = unlog(baseline,constant.added), amplitude.OD = unlog(amplitude,constant.added), 
                             plateau.OD = unlog(plateau,constant.added), inoc.OD = unlog(inoc.log.OD,constant.added), 
                             max.OD = unlog(max.log.OD,constant.added), projected.growth.OD = projected.growth.OD, achieved.growth.OD = achieved.growth.OD,
                             AUC.OD = auc.OD, shape.par = shape.par, shape.par.SE,
                             R.squared = good.fit, RSS = RSS, empty = flag1, asymp.not.reached = flag2, tank = tanking, other = flag3, pdf.file = pdf.file, page.no = page.no)
  }
  
  # Add units to column names
  names2 = names(output.core)
  names2[grep("time",names2)] = sub("$",", hrs", names2[grep("time",names2)])
  names2[grep("rate",names2)] = sub("$",", log.OD/hr", names2[grep("rate",names2)])
  log.OD.fields = c("baseline", "baseline.SE", "amplitude", "amplitude.SE", "plateau", "projected.growth", "achieved.growth", "AUC")
  names2[names2 %in% log.OD.fields] = sub("$", ", log.OD", names2[names2 %in% log.OD.fields])
  names(output.core) = names2
    
  # Add on any additional fields found in the plate layout. 
  all.layout.fields = sapply(fitted.data.set, function(well) unlist(well@well.info)) 
  all.layout.fields = as.data.frame(t(all.layout.fields))
  
  
  addl.info = all.layout.fields[,!(names(all.layout.fields) %in% c("Strain", "Media"))]
  if(!is.data.frame(addl.info)){
    addl.info = data.frame(addl.info)
    names(addl.info) = names(all.layout.fields)[!(names(all.layout.fields) %in% c("Strain", "Media"))] 
  }
  
  output = cbind(output.core,addl.info)
  
  return(output)
}






