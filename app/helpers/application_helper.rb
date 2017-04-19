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
module ApplicationHelper

  def relative_path(fullpath)
    fullpath.to_s.gsub(Rails.root.to_s + "/public","")
  end

  #find all directories in generatedFiles/ and remove
  #any that are more than a day old
  def remove_old_files(path)
  	cur_date = extract_date(path)
  	dirs = Dir.glob(Rails.root + "public/generatedFiles/*") + Dir.glob(Rails.root + "public/uploadedFiles/*")
  	dirs.compact.each do |dir|
  		date = extract_date(dir)
      if(date != cur_date)
        puts "Removing: " + dir #log deletions
  			%x[ rm -rf #{dir} ]
	    end
	end
  end

  private

    def extract_date(path)
  	  part = "Files/"
  	  segment = path.partition(part).last
  	  date = segment.split(/-[0-9]+/).first
    end


end
