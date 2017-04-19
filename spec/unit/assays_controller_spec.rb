require "rails_helper"
require "spec_helper"
require "assays_controller"

describe AssaysController, :type => :controller do
  it "should catch invalid form input" do
    params = {
     :input_file => fixture_file_upload('spec/fixtures/files/samplefile.jpg','image/jpg') 
    }
    get :create, {:assay=>params} 
    expect(:response).to render_template(:new)
  end

  it "render error page if input file is not valid" do
    params = {
     :input_file => fixture_file_upload('spec/fixtures/files/cheat_file.csv','text/csv'),
     :blank_value => 'zero',
     :model => 'sigmoid',
     :plate_type => 'single',
     :growth_threshold => '0.1'
    }
    get :create, {:assay=>params} 
    expect(:response).to render_template(:inputfile_error_message)
  end
  
  it "renders result if everything is correct" do
    params = {
     :input_file => fixture_file_upload('spec/fixtures/files/single_plate_input.csv','text/csv'),
     :blank_value => 'zero',
     :model => 'sigmoid',
     :plate_type => 'single',
     :growth_threshold => '0.1'
    }
    get :create, {:assay=>params} 
    expect(:response).to render_template(:show)
  end

end
