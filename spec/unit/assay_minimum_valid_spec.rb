require "rails_helper"
require "spec_helper"
require "assay"

describe Assay do
  before(:each) do
    @assay = Assay.new
  end
  
  context "Minimum valid assay" do  
  it "shoule be valid" do
     @assay.input_file = fixture_file_upload('spec/fixtures/files/cheat_file.csv','text/csv')
     @assay.blank_value = 'zero'
     @assay.model = 'sigmoid'
     @assay.plate_type = 'single'
     @assay.growth_threshold = '0.1'
     @assay.valid?
     puts @assay.errors.full_messages
     expect(@assay.valid?).to be true
  end
  end
end
