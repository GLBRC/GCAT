require 'rails_helper'

feature 'User fills out all fields' , js:true do

  scenario 'some fields are not valid'do
    visit new_assay_path
    choose('assay_blank_value_zero')

    choose('assay_transformation_user')
    fill_in 'assay_transformation_input', with: '-0.2' #intentionaly invalid

    attach_file('assay_input_file', Rails.root + 'spec/fixtures/files/single_plate_input.csv')
    attach_file('assay_layout_file', Rails.root + 'spec/fixtures/files/single_plate_layout.csv')
    click_button 'Create Assay'

    expect(page).to have_text 'Invalid value for Delta'
  end

  scenario 'all fields are valid, file is valid'do
    visit new_assay_path
    choose('assay_blank_value_zero')

    choose('assay_transformation_user')
    fill_in 'assay_transformation_input', with: '0.1'

    attach_file('assay_input_file', Rails.root + 'spec/fixtures/files/single_plate_input.csv')
    attach_file('assay_layout_file', Rails.root + 'spec/fixtures/files/single_plate_layout.csv')
    click_button 'Create Assay'

    expect(page).to have_text 'Analysis Results'
  end

  scenario 'passes validation, but bad file'do
    visit new_assay_path
    choose('assay_blank_value_zero')

    choose('assay_transformation_user')
    fill_in 'assay_transformation_input', with: '0.1'

    attach_file('assay_input_file', Rails.root + 'spec/fixtures/files/cheat_file.csv')
    attach_file('assay_layout_file', Rails.root + 'spec/fixtures/files/single_plate_layout.csv')
    click_button 'Create Assay'

    expect(page).to have_text 'Error'
  end

end

