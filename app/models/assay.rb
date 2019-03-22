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
require 'zip'
# require 'zip/zipfilesystem'
require 'fileutils'
include FileUtils

# single-plate timestamp
SECONDS = "1/3600".to_r.to_f
MAX_FILE_SIZE_MB = 10

class GCATError < ArgumentError
  attr_accessor :data
  def initialize(data=nil, message=nil)
    super(message)
    @data = data
  end
end

class Assay

  include ActiveModel::Validations
  include ActiveModel::Conversion
  extend ActiveModel::Naming
  
  def initialize(attributes = {})
    if attributes.nil?
      return
    end
    attributes.each do |name, value|
      send("#{name}=", value)
    end
  end

  def persisted?  #
    false
  end
  
  attr_accessor :input_file, 
    :layout_file,
    :blank_value, :blank_value_input, 
    :start_index, 
    :remove_points, 
    :remove_jumps, 
    :plate_type,
    :plate_dimensions_row, :plate_dimensions_column, 
    :timestamp_format, 
    :growth_threshold, 
    :content_type, 
    :model, 
    :loess_input, 
    :console_out, 
    :specg_min, :specg_max, :totg_min, :totg_max, :totg_OD_min, :totg_OD_max, :lagT_min, :lagT_max,
    :transformation, :transformation_input, 
    :area_start_hour, :area_end_hour,
    :uploaded_files_directory, :generated_files_directory, :uniqueID, 
    :input_file_path, :layout_file_path, #pathname object, not string
    :overviewFiles, :pdfFiles, :txtFiles, :consoleOut

  # (1) Validation of input data file
  validates_presence_of :input_file, :message => '- No input file was specified.'
 validates_presence_of :layout_file, :if=>'blank_value=="average"', :message => '- You must upload a layout file for this OD option.'
  # Either look for .csv as file extension, or in mime type (content_type). 
  # No need to validate if there is not filename
  # Change the regex to %r{\.(csv|xlsx)$}i to accept both csv and xlsx files
  validates_format_of :filename, :with => %r{\.(csv)$}i, :message => "- You can only upload csv files.", :unless => lambda { self.input_file == nil }
  
  validate :validate_file_size, :unless => "self.input_file.nil?"

  #simple 'validate_size_of' won't work for both case
  #it will only work for our tests and will fail in real forms
  #this can work is both cases
  def validate_file_size
    if self.input_file.respond_to?(:tempfile)
      self.errors[:input_file] << "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE_MB} MB" unless self.input_file.tempfile.size < MAX_FILE_SIZE_MB.megabytes 
    else 
      self.errors[:input_file] << "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE_MB} MB" unless self.input_file.size < MAX_FILE_SIZE_MB.megabytes 
    end
    unless self.layout_file.nil?
      if self.layout_file.respond_to?(:tempfile)
        self.errors[:layout_file] << "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE_MB} MB" unless self.layout_file.tempfile.size < MAX_FILE_SIZE_MB.megabytes 
      else 
        self.errors[:layout_file] << "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE_MB} MB" unless self.layout_file.size < MAX_FILE_SIZE_MB.megabytes 
      end
    end
  end

  def filename
      self.input_file.original_filename unless self.input_file.nil?
  end
  
  def content_type
      self.content_type = self.input_file.content_type unless self.input_file.nil?
  end
  
  # (2)Validation of transformation
  # if user input is chosen, the user should enter a valid Delta value (A Real Number)
  validates_presence_of :transformation_input, :if => :user_input_r_value?, :message => '- Please Enter Your Delta Value.'
  validates_numericality_of :transformation_input, :if => :user_input_r_value? , :greater_than_or_equal_to => 0 , :message => '- Invalid value for Delta. Please Enter a positive real number.'
  def user_input_r_value?
    transformation == "user"
  end
  
  # (3) Validation of OD blank value
  # if user input is chosen,, the user should enter a valid OD blank value(A Real Number)
  #validates_inclusion_of :blank_value, :in => %w( default user  ), :message => '- Invalid blank value. Please choose one of options'
  validates :blank_value, inclusion:{in: ['default','zero','average','user'], message: "- Invalid Blank Value Choice"}
  validates_presence_of :blank_value_input,  :if => "blank_value==\"user\"", :message => '- Please Enter Your OD Blank Value.'
  validates_numericality_of :blank_value_input, :greater_than_or_equal_to => 0, :if => "blank_value==\"user\"",  :message => '- Invalid OD blank value. Please Enter A Real Number.'
  
  # (4) Validation of start_index, previously (7) innuculation point
  # if user does not enter anything, the system uses the default value start_index = 2
  validates_numericality_of :start_index, :only_integer => true, :greater_than => 0, :message => '- Invalid value for start index. Please Enter A Positive Integer Number', :allow_blank => true
  
  # (5) Validation of remove_points
  # if user does not enter anything, the system uses the default value remove_points = 0, that is an empty list
  #validates_format_of :remove_points,:with => /^[0-9 \,\s]*$/i, :unless => :remove_points_default_value?, :message => '- Please Enter a comma-separated list of points. Example: 2,3,4,5 (Positive Integer Number)'
  validates_format_of :remove_points,:with => /^(\d|\d+\s*,\s*)*$/i, :message => '- Please Enter a comma-separated list of points. Example: 2,3,4,5 (Positive Integer Number)', :allow_blank => true 
  
  # (6) Validation of growth threshold
  validates_numericality_of :growth_threshold, :message => '- Please enter a number.'
  
  #validate plate dimensions
  #v1 not including custom plate dimensions
  #validates :plate_dimensions_column, :numericality => { :only_integer => true, :greater_than => 0}
  #validates :plate_dimensions_row, :numericality => { :only_integer => true, :greater_than => 0 }
  
  # (8) validate heatmap ranges
  HEATMAP_SYMBOLS = [:totg_min, :totg_max, :totg_OD_min, :totg_OD_max, :specg_min, :specg_max, :lagT_min, :lagT_max]
  for s in HEATMAP_SYMBOLS
    validates_numericality_of s, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  end
  
  validates_numericality_of :area_start_hour,  :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :area_end_hour,  :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validate :area_under_curve

  # Validates the area under curve start and end as they are mutually dependent.
  # area_start_hour and area_end_hour must both be >= 0
  # Valid conditions:
  #     area_start_hour = nil, area_end_hour == nil
  #     area_start_hour >=0, area_end_hour == nil
  #     area_start_houe = nil, area_end_hour>=0
  #     0 <= area_start_hour < area_end_hour
  # Note that negativity of these numbers is tested above in 'validates_numericality_of' statements.
  def area_under_curve
    s = self.area_start_hour.to_f unless self.area_start_hour.nil? or self.area_start_hour == ""
    e = self.area_end_hour.to_f unless self.area_end_hour.nil? or self.area_end_hour == ""
    if s and e and s >= e
        self.errors.add(:area_end_hour, "must be greater than start hour.")
        self.errors.add(:area_start_hour, "must be less than end hour.")
    end
  end

  #validation of model
  validates_presence_of :model
  validates :model, inclusion:{in: ['sigmoid','sigmoid-linear','loess'], message:"Invalid model"}
  
  #validation of plate type
  validates :plate_type, inclusion:{in: ['multiple','single'], message:"Invalid plate type"}

  def pad_date(unit)
    unit.to_i < 10 ? "0" + unit.to_s : unit.to_s
  end

  def getUniqueID
    # uniqueID = Process.pid.to_s + "-" + Time.now.to_i.to_s
    #wanted the date to be easier to extract NWD 2/26/14
    today = Time.now
    uniqueID = today.year.to_s + pad_date(today.month) + pad_date(today.day) + "-" + today.to_i.to_s + "-" + rand(36**8).to_s(36)
    #if we want it more randominzed
  end

 
  def generate_directory_names
    uniqueID = getUniqueID
    # set working directories for uploaded and generated files
    @uniqueID = uniqueID
    @uploaded_files_directory = Rails.root.join("public", "uploadedFiles", uniqueID)
    @generated_files_directory = Rails.root.join("public", "generatedFiles", uniqueID)
  end

  #Raise Argument Error is file too large
  def generate_directory_and_move_files
    
    generate_directory_names unless not @uploaded_files_directory.nil?
    
    # make directory and set permission 
    FileUtils.mkdir_p @uploaded_files_directory
    FileUtils.chmod 0755, @uploaded_files_directory
    FileUtils.mkdir_p @generated_files_directory
    FileUtils.chmod 0755, @generated_files_directory
   
    # upload input data file from where it locates into web server via uri/url
    fromfile = self.input_file
    @input_file_path = @uploaded_files_directory.join(fromfile.original_filename)
      #supports testing scripts that directly uploads tempfile
    if fromfile.respond_to?(:tempfile)
      FileUtils.copy( fromfile.tempfile.path, @input_file_path )
    else 
      FileUtils.copy( fromfile.path, @input_file_path )
    end
    
    # upload layout data file from where it locates into web server via uri/url
    unless self.layout_file.nil?
      fromfile = self.layout_file
      @layout_file_path = @uploaded_files_directory.join(fromfile.original_filename)
      if fromfile.respond_to?(:tempfile)
        FileUtils.copy( fromfile.tempfile.path, @layout_file_path )
      else
        FileUtils.copy( fromfile.path, @layout_file_path )
      end
    end
  
  end

  def bind_arguments_to_r
    # use web interface parsed parameters to call R function/library via Rinruby  
    R.eval ('library(GCAT)')
    R.assign "out.dir", @generated_files_directory.to_s
    # That is for Single Plate case. Need modification for multiple plate case
    #Use one set.constants call only!

    #it is possible to post data without the validation from
    #the front-end. So assign it a default value so that we
    #don't have to deal with nil anymore
    self.plate_type ||= 'single'
    if self.plate_type == 'single'
      R.assign 'single.plate', 'T'
      timestamp_format = SECONDS #self.timestamp_format.to_r.to_f
    else
      R.assign 'single.plate', 'F'
      timestamp_format = self.timestamp_format.to_s
    end    
    #################for warringer data###########################################
    # used for custom dims 
    #R.assign 'plate.nrow', 10
    #R.assign 'plate.ncol', 20
    ##############################################################################
    
    R.assign 'time.input', timestamp_format
    #R.eval ("gcat.set.constants(time.format = #{timestamp_format}, plate.nrow = #{self.plate_dimensions_row}, plate.ncol = #{self.plate_dimensions_column})")

    # assign type of file to load.type; ext determines function call by R
    R.assign "load.type", "csv"
    ext = "csv" 

    # (1) input data file
    R.assign "file", @input_file_path.to_s

    first_rows = ["Well positions", "Destination plate name", "Plate ID"]
    begin
      file_row = ""
      File.open(@input_file_path) {|f| file_row = f.readline.split(",").first}
      unless first_rows.include?(file_row)
        raise GCATError.new({:error_message => "Error: Unknown file format.", :path => @input_file_path}),'Unknown file format' 
      end
    rescue
      #bad encoding try to validate in R
      first_rows.collect! {|r| r.gsub(" ", ".")} # convert to R format
      R.eval("test.out <- read.csv(file)")
      R.eval("first_entry <- names(test.out)[1]")     
      
      begin
        #Rinruby will raise exception if it it nil
        R.first_entry
      rescue
        raise GCATError.new({:error_message => "Error: Unknown file format."}),'Unknown file format' 
      end
      
      unless first_rows.include?(R.first_entry)
        raise GCATError.new({:error_message => "Error: Unknown file format."}),'Unknown file format' 
      end
    end

    # (2) transformation. N value (A Real Number)
    # -1 means use user entered value
    # otherwise paese self.transformation directly
    self.transformation ||= '0'
    if self.transformation == 'user'
      R.assign "add.constant", self.transformation_input.to_f
    else
      R.assign "add.constant", self.transformation.to_f
    end

    # (3) blank value (A Real Number)
    self.blank_value ||= 'zero'
    if (self.blank_value == 'default')
      R.eval "blank.value <- NULL"
    elsif (self.blank_value == 'zero')
      R.eval "blank.value <- 0"
    elsif (self.blank_value == 'average')
      R.eval "blank.value <- \"average.layout\""
    elsif (self.blank_value == 'user')
      R.assign "blank.value", self.blank_value_input.to_f
    end

    # (4) start index (A Positive Integer Number).  Cannot be 1 if blank value is default.
    self.start_index ||= '1'
    if (self.blank_value == 'default')
      if (self.start_index == '' or self.start_index.to_i == 1)
        raise GCATError.new({:error_message => "Error: inoculation timepoint cannot be 1 if using first OD reading as blank"}),'inoculation point error'
      end
    end

    if(self.start_index == '')
      R.assign "start.index", 1
    else 
      R.assign "start.index", self.start_index.to_i
    end
    
    # (5) remove points [a space-separated list of points. Example: 2,3,4,5 (Positive Integer Number)]
    ##collect! calls .to_i on each string in the array and replaces the string with the result of the conversion.
    self.remove_points ||= ''
    R.assign "points.to.remove", self.remove_points.gsub(/\s+/,"").split(',').collect! {|n| n.to_i}
    
    self.growth_threshold ||= ''
    R.assign  "growth.cutoff", self.growth_threshold
    
    if (self.layout_file_path == nil)
      R.eval "layout.file <- NULL"
    else
      R.assign "layout.file", self.layout_file_path.to_s
    end
    
    #debugger
    #R.assign "plate.nrow", self.plate_dimensions_row
    #R.assign "plate.ncol", self.plate_dimensions_column
    
    ## (6) remove jumps (true/false)
    self.remove_jumps ||= 0
    if (self.remove_jumps == 1)
      R.eval "remove.jumps <- T"
    else
      R.eval "remove.jumps <- F"
    end
    
    
    # Using growth curve model. By default if this if block
    # is not taken then the Sigmund model is used.
    self.model ||= 'sigmoid'
    if (self.model == 'loess')
      R.assign 'use.loess', 'T'
      # Soothing parameter for growth curve model. Applied for Loess model only.
      if (self.loess_input != "")
        R.assign 'smooth.param', self.loess_input.to_f
      else
        R.assign 'smooth.param', 0.1
      end
    elsif (self.model == 'sigmoid-linear')
      # Currently not in use. May return someday... NWD 9/1
      R.assign 'use.loess', 'F'
    elsif (self.model == 'sigmoid')
      # Initialize values for growth curve models.
      R.assign 'use.loess', 'F'
      R.assign 'smooth.param', 0.1 # default value
    end

    ## Heatmap values
    symbols = [[:specRange, :specg_min, :specg_max], 
      [:totalRange, :totg_min, :totg_max], 
      [:totalODRange, :totg_OD_min, :totg_OD_max], 
      [:lagRange, :lagT_min, :lagT_max]]
    
    for r_range_symbol, ruby_min_symbol, ruby_max_symbol in symbols
      ruby_min = self.send("#{ruby_min_symbol}")
      ruby_max = self.send("#{ruby_max_symbol}")
      if !ruby_min.blank? or !ruby_max.blank?
        puts "#{r_range_symbol} <- c(#{ruby_min.blank? ? "NA" : ruby_min.to_f}, #{ruby_max.blank? ? "NA" : ruby_max.to_f})"
        R.eval "#{r_range_symbol} <- c(#{ruby_min.blank? ? "NA" : ruby_min.to_f}, #{ruby_max.blank? ? "NA" : ruby_max.to_f})" 
      else
        R.eval "#{r_range_symbol} <- NA"
      end
    end
    # Area under curve
    self.area_start_hour ||= ''
    self.area_end_hour ||= ''
    if self.area_start_hour != ""
      R.assign 'auc.start', self.area_start_hour.to_f
    else
      R.eval 'auc.start <- NULL'
    end
    if self.area_end_hour != ""
      R.assign 'auc.end', self.area_end_hour.to_f
    else
      R.eval 'auc.end <- NULL'
    end
  end

  #remove path components before (including) /public/
  #and then strip spaces from path
  def strip_path(path)
    path.gsub(Rails.root.to_s + '/public','')#.gsub(/\s+/, "")
  end

  def categorize_generated_files(files)
      @overviewFiles = []
      @pdfFiles = []
      @txtFiles = []
      @consoleOut = @generated_files_directory.join("console_out.txt").to_s

      for f in files
        if f.include? "_overview.jpg"
          @overviewFiles.push(strip_path(f))
          #overviewFiles = overviewFiles + files[i] +  "\n"
        end

        if f.include? "_plots"
          @pdfFiles.push(strip_path(f))
          #pdfFile = pdfFile + files[i] +  "\n"
        end

        if f.include? ".txt"
          @txtFiles.push(strip_path(f))
          #txtFile = txtFile + files[i] +  "\n"
        end
      end
  end

  def zip_files(files)
    if self.plate_type == 'm'
      @zipfile = @generated_files_directory.join("multiplePlateAnalysis.zip").to_s
    else
      @zipfile = @generated_files_directory.join("singlePlateAnalysis.zip").to_s
    end
    # create Zip files at current directory
    
    Zip::File.open(@zipfile, Zip::File::CREATE) { |zf| 
      for f in files
        zf.add(File.basename(f), f)
      end
      #files.each{|file| zf.add(file.sub(out_dir_path + "/", ""), file))}
      #zf.add(File.basename(consoleOut), consoleOut)
    }
    
  end
    
  def r_calculation 
    # This block evaluates the files (csv or xlsx, single.plate or multiple.plate)

  R.eval 'R_file_return_value <- gcat.analysis.main(
                        file, single.plate, layout.file, out.dir=out.dir, graphic.dir = out.dir, add.constant, blank.value, 
                        start.index, growth.cutoff, use.loess=use.loess, smooth.param=smooth.param, 
                        lagRange = lagRange, totalRange = totalRange, totalODRange = totalODRange, specRange = specRange, 
                        points.to.remove = points.to.remove,remove.jumps,use.linear.param=F, time.input, plate.nrow = 8, 
                        plate.ncol = 12, input.skip.lines = 0, multi.column.headers = c("Plate.ID", "Well", "OD", "Time"), single.column.headers = c("","A1"), 
                        layout.sheet.headers = c("Strain", "Media Definition"), silent = T, verbose = F, return.fit = F, overview.jpgs = T,
                        auc.start=auc.start, auc.end=auc.end
            )'
    # good file returns a list of file path(length is more than 1), bad file returns error message string(array length = 1)
    puts R.R_file_return_value
    
    R.eval ('R_array_return_length <- length(R_file_return_value)')
    
    unless  R.R_array_return_length == 1
      files = R.R_file_return_value  # returns a list of file path
      status = true
    else
      error_message = R.R_file_return_value
      if(error_message.include? "Error in <remove.points>")
        num_data_points = error_message.split("data has ").last.gsub("\n\n", "")
        error_message = "Invalid 'Points to ignore'. Please select a value in the range (1-#{num_data_points})."
      else
        # debugger
        console_message = error_message
        split_s = error_message.split(":")
        print split_s
        split_s.each {|x| error_message = x}
      end
      raise GCATError.new({:error_message => error_message, :console_msg => console_message}), 'Error during calculation'
    end
    
    # process generated files
    raise GCATError.new(), "No files generated" if files.empty?
   
    categorize_generated_files(files)
   
    raise GCATError.new(), "No overview files generated" if @overviewFiles.empty?
    
    zip_files files
      
    #return results unless error
    #zip files, jpg of overviews, txt file for datagrid, pdf file
    return {:status => status, :overviewFiles => @overviewFiles, :zipfile => @zipfile, :txtFile => @txtFiles, :pdfFile => @pdfFiles, :inputfile => @input_file_path, :layout_file => @layout_file_path, :model => @model, :consoleout => @consoleOut}
  end # end of r_calculation method
  
end # class Assay
