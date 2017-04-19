#
# Copyright 2012 The Board of Regents of the University of Wisconsin System.
# Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead,
# Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
#
# This file is part of GCAT.
#
# GCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GCAT.  If not, see <http://www.gnu.org/licenses/>.
#
module AssaysHelper
  
  def date_time_options
    
    [['Y-m-d H:M:S', '%Y-%m-%d %H:%M:%S'],['Y-m-d H:M:S p', '%Y-%m-%d %I:%M:%S %p'],['m/d/y H:M:S','%m/%d/%y %H:%M:%S'],['m/d/y H:M:S p','%m/%d/%y %I:%M:%S %p'],['d/m/y H:M:S','%d/%m/%y %H:%M:%S'],['d/m/y H:M:S p','%d/%m/%y %I:%M:%S %p'],['m/d/Y H:M', '%m/%d/%Y %H:%M']]
    
  end
  
  
end
