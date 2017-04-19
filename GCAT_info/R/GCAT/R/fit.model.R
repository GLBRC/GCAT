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
#                                                                      #
#  Fit a parameterized model to the growth data in a well object.      #
#                                                                      #
# There are now three modelling choices:                               #
#   1) Sigmoid model (no linear param c)                               #
#   2) Linear Sigmoid model                                            #
#   3) Loess (with optional smoothing parameter)                       #
########################################################################
#' fit.model 
#'  
#' This function will use the function stored in the "guess" slot of \code{growth.model} to calculate initial guesses  
#' for growth.model parameters, then it will use the "formula" slot with \code{nls} to fit a non-linear least squares  
#' \code{growth.model} or Local Polynomial Regression Fitting to the data. Richards model is first fitted.  
#' If the shape parameter is statisticaly significant then Richards is used. If it is within 2 SE of 1 or Zero than  
#' a simpler model is preferred. If the Richards fit fails, then Logistic is tried. If it fails, Gompertz is tried. 
#' Model fit failure is reported if none of the models can sucessfully fit the data 
#' 
#' @param input.well The well needed to be fitted with the given model. 
#' @param growth.model What growth model should be used? 
#' @param backup.growth.model If \code{gowth.mode} fails, this model will be used. 
#' @param fit.if.no.growth should the function attempt to fit a well even if there was no growth detected? default is F 
#' @param silent output back to R console? 
#' @param use.linear.param Should an additional linear parameter (c) be used when fitting the data to the model? 
#' @param use.loess Should Local Polynomial Regression Fitting (loess function) be used instead of nls? 
#' @param smooth.param If loess is used, an optional smoothing parameter. Default is .6 
fit.model = function(input.well, growth.model, backup.growth.model = NULL, fit.if.no.growth = F, 
                     use.linear.param=F, use.loess=F, smooth.param, silent = T){
 
  # Conditional breakpoint: go into debugging mode when fitting a specific well
  #if (input.well@position["row"] == "A" && input.well@position["col"] == "12") browser()
 
  # Change all relevant slots to <NA> or blank values
  input.well@model.name = "<NA>"
  input.well@fit.par = list()
  input.well@equation = expression()
 
  # Get OD vs. time data from well
  input.data = data.from(input.well, na.rm = T)
 
  # Skip well if <no.growth> in slot "curve.par" is set to true, and <fit.if.no.growth> is false.
  if(!fit.if.no.growth & lacks.growth(input.well)){
    input.well@fit.info = "skipped - no growth in well."
    if (!silent)
      cat(plate.name(input.well), well.name(input.well), ":", input.well@fit.info, "\n")
    return(input.well)
  }
  # Skip well if there are fewer than 5 data points left in the analysis.
  if (length(input.data$Time) < 5){
    input.well@fit.info = "skipped - not enough points."
    if (!silent)
      cat(plate.name(input.well), well.name(input.well), ":", input.well@fit.info, "\n")
    return(input.well)
  }
       
  # Change column headers of input.data to the more general "Time" vs. "y"
  names(input.data) = c("Time", "y")
 
  #  Set a lower bound for nls model parameters A and b to slightly lower than min(y)
  low.y = min(input.data$y,na.rm=T)
  low.y = low.y - 0.1*abs(low.y)
 
  # Extract the model formula from <growth.model> (slot "formula")
  # Use the function from slot "guess" to calculate initial guesses for model parameters based on slope estimates in <input.well>
  # Attempt to fit a nonlinear least squares odel using <nls>
 
  # Creating loess, logistics, richards, and gompertz model. 
  loess.e = expression("loess")
  loess.f = formula(Time ~ y)
  loess.model = model("local polynomial regression fit.", loess.e, loess.f, loess.g)
  
  ### Testing accessor method for class model.
  #print(getName(loess.model))
  #print(getFormula(loess.model))
  #print(getGuess(loess.model))
  ### End testing ###
  
  remove(loess.e, loess.f)
  
  ########################################################################
  #     Create the logistic 4-parameter model (when v ~ 1)               #
  ########################################################################
  logistic.g = function(well,smooth.param=0.75) {
    loess.model@guess(well,smooth.param)
  }
  
  ########################################################################
  #     Create the Richards 5-parameter model                            #
  ########################################################################
  richards.g =  function(well,smooth.param=0.75){
    c(loess.model@guess(well,smooth.param),v=0.5)
  }
  
  ########################################################################
  #     Create the Gompertz model (might be useful as a                  #
  #     limiting case of Richards model when v ~ 0)                      #
  ########################################################################
  gompertz.g =  function(well,smooth.param=0.75){
    loess.model@guess(well,smooth.param)
  }
  
  logistic.e = expression((A/(1+exp((4*u/A)*(lam-Time)+2)))+b)
  logistic.f = formula(y~(A/(1+exp((4*u/A)*(lam-Time)+2)))+b)
  logistic = model("logistic sigmoid.", logistic.e, logistic.f, logistic.g)
  remove(logistic.e, logistic.f, logistic.g)
  
  richards.e = expression(A*(1+v*exp(1+v)*exp((u/A)*(1+v)**(1+1/v)*(lam-Time)))**(-1/v)+b)
  richards.f = formula(y~A*(1+v*exp(1+v)*exp((u/A)*(1+v)**(1+1/v)*(lam-Time)))**(-1/v)+b)
  richards = model("richards sigmoid", richards.e, richards.f, richards.g)
  remove(richards.e, richards.f, richards.g)
  
  gompertz.e = expression(A*exp(-exp((u*exp(1)/A)*(lam-Time)+1))+b)
  gompertz.f = formula(y~A*exp(-exp((u*exp(1)/A)*(lam-Time)+1))+b)
  gompertz = model("gompertz sigmoid", gompertz.e, gompertz.f, gompertz.g)
  remove(gompertz.e, gompertz.f, gompertz.g)
  
 
  
  # 3) Loess (with optional smoothing parameter)
  if(use.loess){
    number.of.points = nrow(input.well@screen.data)
    if (smooth.param <= 1/number.of.points)
      exception("", "Invalid input: Smoothing parameter is out of range.")
   
    fit = try(loess(y~Time, data=input.data, span=smooth.param), silent=TRUE)
    input.well@loess = fit
    if (class(fit) != "loess") stop("Loess fit failed on well", paste(input.well@position,collapse=" "))
    input.well@fit.info = "Loess model fit successfully."
    input.well@model.name = loess.model@name
    input.well@equation = loess.model@expression
    # There are no estimated params, so just return the initial guesses
    input.well@fit.par = append(as.list(loess.model@guess(input.well,smooth.param)),list("smoothing parameter"=smooth.param))
    # Note: since there are no params there are no Std. Errors either
    input.well@inflection.time = inflection.time(input.well)
    # calculate Rss for loess
    input.well@rss = sum((input.data$y-predict(fit))**2)
  } else {
    fit = fit.nls.model(input.well,richards)
    # should we use richards? Yes, unless the v param is close to 1 or Zero
    if(class(fit) == "nls"){
      rich.fit = fit  # if v is significant or other fits consequently fail
      fit.par = as.list(coef(fit))
      # is fit similar to the Logistic?
      if(fit.par$v >= .5 && abs(fit.par$v-1) < 2*summary(fit)$parameters["v","Std. Error"] ){
        fit = fit.nls.model(input.well,logistic)
        input.well@fit.info = paste("Logistic model fit successfully.")
        input.well@model.name = logistic@name
        input.well@equation = logistic@expression
      # is fit similar to Gompertz?
      }else if(fit.par$v < .5 && abs(fit.par$v) < 2*summary(fit)$parameters["v","Std. Error"]){
        fit = fit.nls.model(input.well,gompertz)
        input.well@fit.info = "Gompertz model fit successfully."
        input.well@model.name = gompertz@name
        input.well@equation = gompertz@expression
      # v param is significant. stick with Richards
      }else{
        input.well@fit.info = paste("Richards model fit successfully.")
        input.well@model.name = richards@name
        input.well@equation = richards@expression
      }
      # just in case logistic or gompertz failed to fit...
      if(class(fit) != "nls"){
        fit = rich.fit
        input.well@fit.info = paste("Richards model fit successfully.")
        input.well@model.name = richards@name
        input.well@equation = richards@expression
      }
    } else{
      # Richards failed. try backup models
      fit = fit.nls.model(input.well,logistic)
      if(class(fit) != "nls"){
        # last resort try gompertz
        fit = fit.nls.model(input.well,gompertz)
        if(class(fit) != "nls"){
          input.well@fit.info = "Model fitting failed." 
        } else{
          input.well@fit.info = "Gompertz model fit successfully."
          input.well@model.name = gompertz@name
          input.well@equation = gompertz@expression
        }
      }else{
        input.well@fit.info = paste("Logistic model fit successfully.")
        input.well@model.name = logistic@name
        input.well@equation = logistic@expression
      }
    }
  }
       
  # If no error was reported by the model fitting, add coefficients to slot "fit.par",
  # Also add the Standard Errors for each parameter
  if (class(fit) == "nls"){
    input.well@nls = fit
    input.well@inflection.time = inflection.time(input.well)
    input.well@fit.par = as.list(coef(fit))
    rSs =  sum(residuals(fit)**2)
      
    if (length(rSs) != 0)
      input.well@rss = rSs
    else 
      input.well@rss = NA
    input.well@fit.std.err = as.list(summary(fit)$parameters[,"Std. Error"])
  }
  # Output to console
  if (!silent)
    cat(plate.name(input.well), well.name(input.well), ":", input.well@fit.info, "\n")
    return(input.well)
}

#'  Fit nls model to a well using a specified model
#'
#' @param input.well object of class well
#' @param  model object of class model, e.g. richards, gompertz or logistic
fit.nls.model <- function (input.well, model) {
  # Get OD vs. time data from well
  input.data = data.from(input.well, na.rm = T)
  # Change column headers of input.data to the more general "Time" vs. "y"
  names(input.data) = c("Time", "y")
 
  #  Set a lower bound for nls model parameters A and b to slightly lower than min(y)
  low.y = min(input.data$y,na.rm=T)
  low.y = low.y - 0.1*abs(low.y)
 
  #  Set the initial guess
  start = model@guess(input.well)
 
  #  Set lower bounds
  if (length(start) == 4) {
    lower = c(low.y,low.y,0,0)
  } else if (length(start) == 5) {
    lower = c(low.y,low.y,0,0,0.07)
  } else {
    stop("Unsupported model: ", model@name)
  }
  #  Make sure initial guess values do not violate lower bounds
  start[start < lower] = lower[start < lower]
 
  #  Fit the model
  try(nls(formula = model@formula, data = input.data, start = start, algorithm="port", lower=lower), silent = TRUE)
}
