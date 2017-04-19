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

# Wrapper for sapply to use lapply over an array, conserving the dimensions.
aapply = function(x, FUN,...){
  dim.values = dim(x)
	dim.names = dimnames(x)
	x = lapply(x, function(x){FUN(x,...)})
	dim(x) = dim.values
	dimnames(x) = dim.names
	return(x)
	}

# A function to manually create an unchecked exception.
exception = function(class, msg)
{
    cond <- simpleError(msg)
    class(cond) <- c(class, "MyException", class(cond))
    stop(cond)
}
