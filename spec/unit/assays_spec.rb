require "rails_helper"
require "spec_helper"
require "assay"

describe Assay  do
  before(:each) do
    @assay = Assay.new
  end
 
context "Validation" do  
#########################################################
#begin of context Input File
  NO_INPUT_FILE = '- No input file was specified.'
  ONLY_CSV_FILES = '- You can only upload csv files.'
  REQUIRE_LAYOUT_FILE = '- You must upload a layout file for this OD option.'
  context "Input File" do
    it "should warn if input_file is not provided" do
      expect(@assay.valid?).to be false
      expect(@assay.errors[:input_file]).to include NO_INPUT_FILE
    end

    it "requires layout file if user choose to use average OD of blank wells at each time point as OD" do
      f = fixture_file_upload('spec/fixtures/files/samplefile.jpg','image/jpg')
      @assay.input_file = f
      @assay.blank_value = "average"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:layout_file]).to include REQUIRE_LAYOUT_FILE
    end
  
    it "shoule not accept .jpg file" do
      f = fixture_file_upload('spec/fixtures/files/samplefile.jpg','image/jpg')
      @assay.input_file = f
      expect(@assay.valid?).to be false
      expect(@assay.errors[:filename]).to include ONLY_CSV_FILES
    end

    it "should accept .csv file" do
      f = fixture_file_upload('spec/fixtures/files/samplefile.csv','text/csv')
      @assay.input_file = f
      expect(@assay.valid?).to be false
      expect(@assay.errors[:filename].first).to be nil
    end
    
    it "should not allow input file greater than 10 mb" do
      @assay.input_file = fixture_file_upload('spec/fixtures/files/large_file.csv','text/csv')
      expect(@assay.valid?).to be false
      expect(@assay.errors[:input_file]).to include 'Error: File too big. Maximum file size allowed is 10 MB'
    end
    
    it "should not allow layout file greater than 10 mb" do
      @assay.input_file = fixture_file_upload('spec/fixtures/files/samplefile.csv','text/csv')
      @assay.layout_file = fixture_file_upload('spec/fixtures/files/large_file.csv','text/csv')
      expect(@assay.valid?).to be false
      expect(@assay.errors[:layout_file]).to include 'Error: File too big. Maximum file size allowed is 10 MB'
    end
  end
#end of context Input File
#########################################################

#########################################################
#begin of plate type
  context "plate type" do
    BAD_PLATE_TYPE = 'Invalid plate type'
    it "should accept single" do
      @assay.plate_type = 'single'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:plate_type]).not_to include BAD_PLATE_TYPE
    end

    it "should accept multiple" do
      @assay.plate_type = 'multiple'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:plate_type]).not_to include BAD_PLATE_TYPE
    end

    it "should not accept invalid plate type" do
      @assay.plate_type = 'something'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:plate_type]).to include BAD_PLATE_TYPE
    end
    
    it "should not accept empty plate type" do
      @assay.plate_type = ''
      expect(@assay.valid?).to be false
      expect(@assay.errors[:plate_type]).to include BAD_PLATE_TYPE
    end
    
    it "should not accept nil plate type" do
      @assay.plate_type = nil
      expect(@assay.valid?).to be false
      expect(@assay.errors[:plate_type]).to include BAD_PLATE_TYPE
    end

  end
#end of plate type
#########################################################

#########################################################
#begin of context OD Transform
  NO_DELTA_VALUE = '- Please Enter Your Delta Value.'  
  NEG_DELTA_VALUE = '- Invalid value for Delta. Please Enter a positive real number.'
  
  context "OD Transform" do
    context "User choose log(x+delta)" do
      before(:each) do
        @assay.transformation = "user"
      end

      it "should warn if delta is not provided" do
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).to include NO_DELTA_VALUE
      end
      
      it "should not accept empty delta" do
        @assay.transformation_input = ""
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).to include NO_DELTA_VALUE
      end

      it "should not accept negative delta" do
        @assay.transformation_input = "-0.23"
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).to include NEG_DELTA_VALUE
      end
      
      it "should accept 0.23 for delta" do
        @assay.transformation_input = "0.23"
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NO_DELTA_VALUE
        expect(@assay.errors[:transformation_input]).not_to include NEG_DELTA_VALUE
      end
    end

    context "User select log(x)" do
      before(:each) do
        @assay.transformation = "0"
      end

      it "should not warn about delta not provided" do
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NO_DELTA_VALUE
      end

      it "should accept empty delta" do
        @assay.transformation_input = ""
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NO_DELTA_VALUE
      end
      
      it "should accept negative delta" do
        @assay.transformation_input = "-0.23"
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NEG_DELTA_VALUE
      end
    end

    context "User select log(x+0.1)" do
      before(:each) do
        @assay.transformation = "01"
      end

      it "should not warn about delta not provided" do
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NO_DELTA_VALUE
      end

      it "should accept empty delta" do
        @assay.transformation_input = ""
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NO_DELTA_VALUE
      end
      
      it "should accept negative delta" do
        @assay.transformation_input = "-0.23"
        expect(@assay.valid?).to be false
        expect(@assay.errors[:transformation_input]).not_to include NEG_DELTA_VALUE
      end
    end
  end
#end of context od transform
#########################################################

#########################################################
#begin of context od blank
  context "OD Blank" do
    NO_OD_BLANK = '- Please Enter Your OD Blank Value.'
    BAD_OD_BLANK = '- Invalid OD blank value. Please Enter A Real Number.'

    context "User input" do
      before(:each) do
        @assay.blank_value = "user"
      end

      it "should warn about value not provided" do
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).to include NO_OD_BLANK
      end

      it "should warn about value is empty" do
        @assay.blank_value_input = ''
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).to include NO_OD_BLANK
      end

      it "should not accept non-real number" do
        @assay.blank_value_input = 'a'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).to include BAD_OD_BLANK
      end

      it "should not accept negative value" do
        @assay.blank_value_input = '-0.23'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).to include BAD_OD_BLANK
      end
      
      it "should accept 0" do
        @assay.blank_value_input = '0'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
      
      it "should accept 0.23" do
        @assay.blank_value_input = '0.23'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
    end

    context "Not User Input" do
      before(:each) do
        @assay.blank_value = "something else"
      end

      it "should not warn about value not provided" do
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include NO_OD_BLANK
      end

      it "should not warn about value is empty" do
        @assay.blank_value_input = ''
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include NO_OD_BLANK
      end

      it "should accept non-real number" do
        @assay.blank_value_input = 'a'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
      
      it "should accept negative value" do
        @assay.blank_value_input = '-0.23'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
      
      it "should accept 0" do
        @assay.blank_value_input = '0'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
      
      it "should accept 0.23" do
        @assay.blank_value_input = '0.23'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:blank_value_input]).not_to include BAD_OD_BLANK
      end
    end
  end
#end of context od blank
#########################################################

#########################################################
#start of start index
  context "Start Index" do
    BAD_START_IDX = '- Invalid value for start index. Please Enter A Positive Integer Number'

    context "User input" do
      before(:each) do
        @assay.start_index = "not_empty"
      end

      it "should not accept non-real number" do
        @assay.start_index = 'a'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:start_index]).to include BAD_START_IDX
      end

      it "should not accept negative integer" do
        @assay.start_index = '-1'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:start_index]).to include BAD_START_IDX
      end
      
      it "should not accept decimals" do
        @assay.start_index = '1.2'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:start_index]).to include BAD_START_IDX
      end
      
      it "should not accept 0" do
        @assay.start_index = '0'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:start_index]).to include BAD_START_IDX
      end
      
      it "should accept 1" do
        @assay.start_index = '1'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:start_index]).not_to include BAD_START_IDX
      end
    end

    context "Not Provided" do
      before(:each) do
        @assay.blank_value = ""
      end
    end
  end
#end of start index
#########################################################

#########################################################
#start of model
  context "model" do
    BAD_MODEL = 'Invalid model'
    it "should accept sigmoid" do
      @assay.model = 'sigmoid'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:model]).not_to include BAD_MODEL
    end
    
    it "should accept sigmoid-linear" do
      @assay.model = 'sigmoid-linear'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:model]).not_to include BAD_MODEL
    end
    
    it "should accept loess" do
      @assay.model = 'loess'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:model]).not_to include BAD_MODEL
    end
    
    it "should not accept invalid model" do
      @assay.model = 'invalid'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:model]).to include BAD_MODEL
    end
    
    it "should not accept empty model" do
      @assay.model = ''
      expect(@assay.valid?).to be false
      expect(@assay.errors[:model]).to include BAD_MODEL
    end
  end
#end of model
#########################################################


#########################################################
#start of remove points
  context "Remove Points" do
    BAD_REMOVE_POINTS = '- Please Enter a comma-separated list of points. Example: 2,3,4,5 (Positive Integer Number)'

    context "User input" do
      before(:each) do
        @assay.remove_points = "not_empty"
      end

      it "should not accept dot seperated numbers" do
        @assay.remove_points = '1.2.3.4.5.6.7'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end
      
      it "should not accept space seperated numbers" do
        @assay.remove_points = '1 2 3 4 5 6 7'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end

      it "should not accept letters - 1" do
        @assay.remove_points = '1,2,3,a,5,6'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end
      
      it "should not accept letters - 2" do
        @assay.remove_points = 'a,2,3,4,5,6'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end
      
      it "should not accept letters - 3" do
        @assay.remove_points = '1,2,3,4,5,a'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end
      
      it "should not accept consecutive commas" do
        @assay.remove_points = '1,2,3,4,,5,6'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).to include BAD_REMOVE_POINTS
      end

      it "should accept variable space padding" do
        @assay.remove_points = '1,2 ,3  ,4 ,  5,    6,    '
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).not_to include BAD_REMOVE_POINTS
      end
      
      it "should accept a good input - 1" do
        @assay.remove_points = '1,2,3,4,5,6'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).not_to include BAD_REMOVE_POINTS
      end
      
      it "should accept a good input - 2" do
        @assay.remove_points = '1,2,3,4,5,6,'
        expect(@assay.valid?).to be false
        expect(@assay.errors[:remove_points]).not_to include BAD_REMOVE_POINTS
      end
    end

    context "Not Provided" do
      before(:each) do
        @assay.blank_value = ""
      end
    end
  end
#end of remove points
#########################################################
#########################################################
#start of remove points
  context "Growth Threshold" do
    BAD_GROWTH_THRESHOLD = '- Please enter a number.'
    
    it "should warn about value not provided" do
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).to include BAD_GROWTH_THRESHOLD
    end
    
    it "should not accept non-number" do
      @assay.growth_threshold = 'a'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).to include BAD_GROWTH_THRESHOLD
    end
    
    it "should accept decimal" do
      @assay.growth_threshold = '1.1'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).not_to include BAD_GROWTH_THRESHOLD
    end
    
    it "should accept negative" do
      @assay.growth_threshold = '-1.1'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).not_to include BAD_GROWTH_THRESHOLD
    end
    
    it "should accept zero" do
      @assay.growth_threshold = '0'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).not_to include BAD_GROWTH_THRESHOLD
    end
    it "should accept positive" do
      @assay.growth_threshold = '2'
      expect(@assay.valid?).to be false
      expect(@assay.errors[:growth_threshold]).not_to include BAD_GROWTH_THRESHOLD
    end
  end
#end of remove points
#########################################################

#########################################################
#start of heatmap ranges & auc validation
  context "Heatmap Ranges & AUC validation" do
    NEG_NUM = '- Please Enter a positive real number.'
    SYMBOLS = [:totg_min,:totg_max,:totg_OD_min,:totg_OD_max,:specg_min,:specg_max,:lagT_min,:lagT_max, :area_start_hour, :area_end_hour]
    context "User Input" do
      before(:each) do
        @assay.blank_value = "user"
      end

      it "should allow blank" do
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).not_to include NEG_NUM
        end
      end
      
      it "should allow zero" do
        for s in SYMBOLS
          @assay.send("#{s}=",'0')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).not_to include NEG_NUM
        end
      end
      
      it "should allow positive integer" do
        for s in SYMBOLS
          @assay.send("#{s}=",'1')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).not_to include NEG_NUM
        end
      end
      
      it "should allow positive decimal" do
        for s in SYMBOLS
          @assay.send("#{s}=",'1.2')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).not_to include NEG_NUM
        end
      end
      
      it "should not allow non-numbers" do
        for s in SYMBOLS
          @assay.send("#{s}=",'a')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).to include NEG_NUM
        end
      end
      
      it "should not allow negative integer" do
        for s in SYMBOLS
          @assay.send("#{s}=",'-1')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).to include NEG_NUM
        end
      end
      
      it "should not allow negative decimal" do
        for s in SYMBOLS
          @assay.send("#{s}=",'-0.2')
        end
        expect(@assay.valid?).to be false
        for s in SYMBOLS
          expect(@assay.errors[s]).to include NEG_NUM
        end
      end
    end
  end
#end of heatmap ranges
#########################################################

#########################################################
#start of area under curve
  context "Area Under Curve" do
    NEG_NUM2 = '- Please Enter a positive real number.'
    END_TOO_SMALL = "must be greater than start hour."
    START_TOO_LARGE = "must be less than end hour."
    before(:each) do 
      @assay.blank_value = "user"
    end

    it "should not allow negative end hour" do
      @assay.area_end_hour = "-3.2"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_end_hour]).to include NEG_NUM2
    end

    it "should not allow end being smaller than start" do
      @assay.area_start_hour = "2"
      @assay.area_end_hour = "1"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_end_hour]).to include END_TOO_SMALL
      expect(@assay.errors[:area_start_hour]).to include START_TOO_LARGE
    end
    
    it "should not allow end being equal to start" do
      @assay.area_start_hour = "1"
      @assay.area_end_hour = "1"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_start_hour]).to include START_TOO_LARGE
      expect(@assay.errors[:area_end_hour]).to include END_TOO_SMALL
    end

    it "should not allow negative start hour" do
      @assay.area_start_hour = "-3.2"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_start_hour]).to include NEG_NUM
    end

    it "should allow two blank values" do
      @assay.area_start_hour = ""
      @assay.area_end_hour = ""
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_start_hour]).to be_empty
      expect(@assay.errors[:area_end_hour]).to be_empty
    end
    
    it "should allow end to be nil while start is not" do
      @assay.area_start_hour = "1"
      @assay.area_end_hour = ""
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_start_hour]).to be_empty
      expect(@assay.errors[:area_end_hour]).to be_empty
    end
    
    it "should allow start to be nil while end is not" do
      @assay.area_start_hour = ""
      @assay.area_end_hour = "1"
      expect(@assay.valid?).to be false
      expect(@assay.errors[:area_start_hour]).to be_empty
      expect(@assay.errors[:area_end_hour]).to be_empty
    end

  end
#end of area under curve
#########################################################
end

context "R_Calculation" do
  before(:each) do
    #skip #this may take too much time
  end
  
  after(:each) do
    FileUtils.rm_rf(Rails.root.join('tmp','testing'))
  end

  context "generate directory and move files" do
    before(:each) do 
      @assay.blank_value = "zero"
    end
    
    it "should copy input file and layout file to the new directory" do
      uniqueid = "testing"
      @assay.input_file = fixture_file_upload('spec/fixtures/files/single_plate_input.csv','test/csv')
      @assay.layout_file = fixture_file_upload('spec/fixtures/files/single_plate_layout.csv','test/csv')
      @assay.uploaded_files_directory = Rails.root.join("tmp/testing/uploadedfiles", uniqueid)
      @assay.generated_files_directory = Rails.root.join("tmp/testing/generatedfiles", uniqueid)
      @assay.generate_directory_and_move_files
      expect(File.exist?(@assay.uploaded_files_directory)).to be true
      expect(File.exist?(@assay.generated_files_directory)).to be true
    end

  end

  context "argument binding" do
    before(:each) do
      #skip
      #R = RinRuby.new(false)
      R.echo(enable=false, stderr=nil)
      @assay.blank_value = "zero"
      @assay.generated_files_directory = "random"
      @assay.plate_type = 'single'
      @assay.timestamp_format = "%Y-%m-%d %I:%M:%S %p"
      @assay.input_file_path = 'spec/fixtures/files/single_plate_input.csv'
      @assay.layout_file_path = nil
      @assay.transformation = '0'
      @assay.blank_value = nil
      @assay.start_index = 0
      @assay.remove_points = ""
      @assay.growth_threshold = 0
      @assay.remove_jumps = true
      @assay.model = 'sigmoid'
      @assay.loess_input = ""
      symbols = [[:specg_min, :specg_max], 
        [:totg_min, :totg_max], 
        [:totg_OD_min, :totg_OD_max], 
        [:lagT_min, :lagT_max]]
      for s1, s2 in symbols
        @assay.send("#{s1}=","")
        @assay.send("#{s2}=","")
      end
      @assay.area_start_hour = nil
      @assay.area_end_hour = nil
    end

    after(:each) do
      #R.quit
    end
    
    it "binds out.dir correctly" do
      @assay.generated_files_directory = "some directory"
      @assay.bind_arguments_to_r
      expect(R.pull('out.dir')).to eq "some directory"
    end
  
    it "binds single.plate correctly - 1" do
      @assay.plate_type = 'single'
      @assay.bind_arguments_to_r
      expect(R.pull('single.plate')).to eq "T" 
    end
  
    it "binds single.plate correctly - 2" do
      @assay.plate_type = 'multiple'
      @assay.bind_arguments_to_r
      expect(R.pull('single.plate')).to eq "F" 
    end
    
    it "binds time.input correctly - 1" do
      @assay.plate_type = 'single'
      @assay.bind_arguments_to_r
      expect(R.pull('time.input')).to eq SECONDS
    end
    
    it "binds time.input correctly - 2" do
      @assay.plate_type = 'multiple'
      @assay.bind_arguments_to_r
      expect(R.pull('time.input')).to eq "%Y-%m-%d %I:%M:%S %p"
    end

    it "binds load.type correctly" do
      @assay.bind_arguments_to_r
      expect(R.pull('load.type')).to eq 'csv'
    end

    it "binds file correctly" do
      @assay.bind_arguments_to_r
      expect(R.pull('file')).to eq @assay.input_file_path
    end

    it "tries to verify input data file" do
      @assay.input_file_path = 'spec/fixtures/files/bad_file.csv'
      expect{@assay.bind_arguments_to_r}.to raise_error(GCATError)
    end

=begin
    it "disallows non-valid file" do
      @assay.input_file_path = 'spec/fixtures/files/bad_file_2.csv'
      expect{@assay.bind_arguments_to_r}.to raise_error(ArgumentError)
    end
=end

    it "allows good file" do
      @assay.input_file_path = 'spec/fixtures/files/single_plate_input.csv'
      expect{@assay.bind_arguments_to_r}.not_to raise_error
    end

    it "binds add.constant correctly" do
      @assay.transformation = 'user'
      @assay.transformation_input = '3.2'
      @assay.bind_arguments_to_r
      expect(R.pull('add.constant')).to eq 3.2
    end
    
    it "binds add.constant correctly - 2" do
      @assay.transformation = '0.1'
      @assay.transformation_input = '3.2'
      @assay.bind_arguments_to_r
      expect(R.pull('add.constant')).to eq 0.1
    end

    it "binds blank.value correctly - 1" do
      @assay.blank_value = 'default'
      #work around limitation of rinruby
      #rinruby cannot pull NULL, need to change it to -1
      #instead
      @assay.bind_arguments_to_r
      R.eval <<EOF
        if (is.null(blank.value)){
          blank.value <- -1
        }
EOF
      expect(R.pull('blank.value')).to eq -1
    end


    it "binds blank.value correctly - 1" do
      @assay.blank_value = 'user'
      @assay.blank_value_input = '0.123'
      @assay.bind_arguments_to_r
      expect(R.pull('blank.value')).to eq 0.123
    end
    
    it "binds blank.value correctly - 2" do
      @assay.blank_value = 'zero'
      @assay.bind_arguments_to_r
      expect(R.pull('blank.value')).to eq 0
    end
    
    it "binds blank.value correctly - 3" do
      @assay.blank_value = 'average'
      @assay.bind_arguments_to_r
      expect(R.pull('blank.value')).to eq 'average.layout'
    end

    it "cannot allow start index to be 1 when blank value is default" do
      @assay.blank_value = 'default'
      @assay.start_index = ''
      expect{@assay.bind_arguments_to_r}.to raise_error(GCATError)
    end
    
    it "cannot allow start index to be 1 when blank value is default - 2" do
      @assay.blank_value = 'default'
      @assay.start_index = '1'
      expect{@assay.bind_arguments_to_r}.to raise_error(GCATError)
    end

    it "binds start index correctly" do
      @assay.blank_value = 'zero'
      @assay.start_index = '3'
      @assay.bind_arguments_to_r
      expect(R.pull('start.index')).to eq 3
    end
    
    it "binds start index correctly - 2" do
      @assay.blank_value = 'zero'
      @assay.start_index = ''
      @assay.bind_arguments_to_r
      expect(R.pull('start.index')).to eq 1
    end

    it "binds remove points correctly" do
      @assay.remove_points = "1,   \n 2 , \r 3,  \n  4,   5,"
      @assay.bind_arguments_to_r
      expect(R.pull('points.to.remove')).to eq [1,2,3,4,5]
    end

    it "binds growth cutoff correctly" do
      @assay.growth_threshold = '1'
      @assay.bind_arguments_to_r
      expect(R.pull('growth.cutoff')).to eq '1'
    end

    it "binds layout file correctly - 1" do
      @assay.layout_file_path = nil
      #work around limitation of rinruby
      #rinruby cannot pull NULL, need to change it to -1
      #instead
      @assay.bind_arguments_to_r
      R.eval <<EOF
        if (is.null(layout.file)){
          layout.file <- -1
        }
EOF
      expect(R.pull('layout.file')).to eq -1
    end

    it "binds layout file correctly - 2" do
      @assay.layout_file_path = 'spec/fixtures/files/single_plate_layout.csv'
      @assay.bind_arguments_to_r
      expect(R.pull('layout.file')).to eq 'spec/fixtures/files/single_plate_layout.csv'
    end

    it "binds remove.jumps correctly - 1" do
      @assay.remove_jumps = 1
      #work around limitation of rinruby
      #rinruby cannot pull boolean, need to change it to 0
      #instead
      @assay.bind_arguments_to_r
      R.eval <<EOF
        if (identical(remove.jumps,TRUE)){
          remove.jumps <- 1
        }else{
          remove.jumps <- 0
        }
EOF
      expect(R.pull('remove.jumps')).to eq 1
    end
    
    it "binds remove.jumps correctly - 2" do
      @assay.remove_jumps = 0
      #work around limitation of rinruby
      #rinruby cannot pull boolean, need to change it to 0
      #instead
      @assay.bind_arguments_to_r
      R.eval <<EOF
        if (identical(remove.jumps,FALSE)){
          remove.jumps <- 0
        }else{
          remove.jumps <- 1
        }
EOF
      expect(R.pull('remove.jumps')).to eq 0
    end

    it "binds models correctly - loess input" do
      @assay.model = 'loess'
      @assay.loess_input = ""
      @assay.bind_arguments_to_r
      #workaround
      R.eval <<EOF
        if (use.loess){
          use.loess <- 1
        }else{
          use.loess <- 0
        }
EOF
      expect(R.pull('use.loess')).to eq 1
      expect(R.pull('smooth.param')).to eq 0.1
    end
    
    it "binds models correctly - loess default" do
      @assay.model = 'loess'
      @assay.loess_input = '0.2'
      @assay.bind_arguments_to_r
      #workaround
      R.eval <<EOF
        if (use.loess){
          use.loess <- 1
        }else{
          use.loess <- 0
        }
EOF
      expect(R.pull('use.loess')).to eq 1
      expect(R.pull('smooth.param')).to eq 0.2
    end

#currently unused
    it "binds models correctly - model is sigmoid-linear" do
      @assay.model = 'sigmoid-linear'
      @assay.bind_arguments_to_r
      #workaround
      R.eval <<EOF
        if (use.loess){
          use.loess <- 1
        }else{
          use.loess <- 0
        }
EOF
      expect(R.pull('use.loess')).to eq 0
    end
    
    it "binds models correctly - sigmoid" do
      @assay.model = 'sigmoid'
      @assay.bind_arguments_to_r
      #workaround
      R.eval <<EOF
        if (use.loess){
          use.loess <- 1
        }else{
          use.loess <- 0
        }
EOF
      expect(R.pull('use.loess')).to eq 0
      expect(R.pull('smooth.param')).to eq 0.1
    end
  
    it "binds heatmap ranges correctly - both values valid" do
      symbols = [[:specRange, :specg_min, :specg_max], 
        [:totalRange, :totg_min, :totg_max], 
        [:totalODRange, :totg_OD_min, :totg_OD_max], 
        [:lagRange, :lagT_min, :lagT_max]]
      
      for r_range, ruby_min, ruby_max in symbols
        @assay.send("#{ruby_min}=","0")
        @assay.send("#{ruby_max}=","1")
      end
      
      @assay.bind_arguments_to_r
      
      for r_range, ruby_min, ruby_max in symbols
        expect(R.pull("#{r_range}[1]")).to eq 0
        expect(R.pull("#{r_range}[2]")).to eq 1
      end
    end
    
    it "binds heatmap ranges correctly - min value specified, max value blank" do
      symbols = [[:specRange, :specg_min, :specg_max], 
        [:totalRange, :totg_min, :totg_max], 
        [:totalODRange, :totg_OD_min, :totg_OD_max], 
        [:lagRange, :lagT_min, :lagT_max]]
      
      for r_range, ruby_min, ruby_max in symbols
        @assay.send("#{ruby_min}=",3)
        @assay.send("#{ruby_max}=","")
      end 
     
      @assay.bind_arguments_to_r
      
      for r_range, ruby_min, ruby_max in symbols
        #workaround
        R.eval <<EOF
          if (is.na(#{r_range}[2])){
            #{r_range}[2] <- 0
          }else{
            #{r_range}[2] <- 1
          }
EOF
        expect(R.pull("#{r_range}")).to eq [3,0]
      end
    end


    it "binds heatmap ranges correctly - min value blank, max value specified" do
      symbols = [[:specRange, :specg_min, :specg_max], 
        [:totalRange, :totg_min, :totg_max], 
        [:totalODRange, :totg_OD_min, :totg_OD_max], 
        [:lagRange, :lagT_min, :lagT_max]]
      
      for r_range, ruby_min, ruby_max in symbols
        @assay.send("#{ruby_min}="," ")
        @assay.send("#{ruby_max}=","3")
      end 
     
      @assay.bind_arguments_to_r
      
      for r_range, ruby_min, ruby_max in symbols
        #workaround
        R.eval <<EOF
          if (is.na(#{r_range}[1])){
            #{r_range}[1] <- 0
          }else{
            #{r_range}[1] <- 1
          }
EOF
        expect(R.pull("#{r_range}[1]")).to eq 0
        expect(R.pull("#{r_range}[2]")).to eq 3
      end
    end
    
    it "binds heatmap ranges correctly - both blank" do
      symbols = [[:specRange, :specg_min, :specg_max], 
        [:totalRange, :totg_min, :totg_max], 
        [:totalODRange, :totg_OD_min, :totg_OD_max], 
        [:lagRange, :lagT_min, :lagT_max]]
      
      for r_range, ruby_min, ruby_max in symbols
        @assay.send("#{ruby_min}=","  ")
        @assay.send("#{ruby_max}=","  ")
      end 
     
      @assay.bind_arguments_to_r
      
      for r_range, ruby_min, ruby_max in symbols
        #workaround
        R.eval <<EOF
          if (is.na(#{r_range})){
            #{r_range} <- 0
          }else{
            #{r_range} <- 1
          }
EOF
        expect(R.pull("#{r_range}")).to eq 0
      end
    end

    it "binds auc.start and auc.end correctly - 1" do
      @assay.area_start_hour = '1'
      @assay.area_end_hour = '2'
      @assay.bind_arguments_to_r
      expect(R.pull("auc.start")).to eq 1
      expect(R.pull("auc.end")).to eq 2
    end
    
    it "binds auc.start and auc.end correctly - 2" do
      @assay.area_start_hour = ""
      @assay.area_end_hour = ""
      @assay.bind_arguments_to_r
      R.eval <<EOF
          if (is.null(auc.start)){
            auc.start <- 0
          }else{
            auc.start <- 1
          }
          if (is.null(auc.end)){
            auc.end <- 0
          }else{
            auc.end <- 1
          }
EOF
      expect(R.pull("auc.start")).to eq 0
      expect(R.pull("auc.end")).to eq 0
    end
    
    it "binds auc.start and auc.end correctly - 2" do
      @assay.area_start_hour = nil
      @assay.area_end_hour = nil
      @assay.bind_arguments_to_r
      R.eval <<EOF
          if (is.null(auc.start)){
            auc.start <- 0
          }else{
            auc.start <- 1
          }
          if (is.null(auc.end)){
            auc.end <- 0
          }else{
            auc.end <- 1
          }
EOF
      expect(R.pull("auc.start")).to eq 0
      expect(R.pull("auc.end")).to eq 0
    end

  end
end


context "Analyze R Generated Values" do
  after(:each) do
    FileUtils.rm_rf(Rails.root.join('tmp','testing'))
  end
  
  it "strips path correctly" do
    path = Rails.root.to_s + '/public/A/ B/ c .txt'
    expect(@assay.strip_path(path)).to eq "/A/ B/ c .txt"
  end

  it "categorizes file correctly" do
    uniqueid = 'testing'
    @assay.generated_files_directory = Rails.root.join("tmp/testing/generatedfiles", uniqueid)
    files = [
      Rails.root.to_s + '/public/A/B/ t1 .txt',
      Rails.root.to_s + '/public/A/B/ o1 _overview.jpg',
      Rails.root.to_s + '/public/A/B/ p1 _plots.pdf',
      Rails.root.to_s + '/public/A/ t2 .txt',
      Rails.root.to_s + '/public/ o2 _overview.jpg',
      Rails.root.to_s + '/public/ p2 _plots.pdf'
    ]
    @assay.categorize_generated_files files
    expect(@assay.txtFiles).to include "/A/B/ t1 .txt"
    expect(@assay.overviewFiles).to include "/A/B/ o1 _overview.jpg"
    expect(@assay.pdfFiles).to include "/A/B/ p1 _plots.pdf"
    expect(@assay.txtFiles).to include "/A/ t2 .txt"
    expect(@assay.overviewFiles).to include "/ o2 _overview.jpg"
    expect(@assay.pdfFiles).to include "/ p2 _plots.pdf"
    expect(@assay.consoleOut).to eq @assay.generated_files_directory.join("console_out.txt").to_s
  end
  
  it "generates correct zip files - multiple" do
    uniqueid = 'testing'
    @assay.plate_type = 'm'
    @assay.generated_files_directory = Rails.root.join("tmp/testing/generatedfiles", uniqueid)
    FileUtils.mkdir_p @assay.generated_files_directory
    @assay.zip_files [Rails.root.join("spec/fixtures/files/samplefile.jpg")]
    expect(File.exist?(@assay.generated_files_directory.join("multiplePlateAnalysis.zip"))).to be true
  end

  it "generates correct zip files - single" do
    uniqueid = 'testing'
    @assay.plate_type = 's'
    @assay.generated_files_directory = Rails.root.join("tmp/testing/generatedfiles", uniqueid)
    FileUtils.mkdir_p @assay.generated_files_directory
    @assay.zip_files [Rails.root.join("spec/fixtures/files/samplefile.jpg")]
    expect(File.exist?(@assay.generated_files_directory.join("singlePlateAnalysis.zip"))).to be true
  end
end

end
