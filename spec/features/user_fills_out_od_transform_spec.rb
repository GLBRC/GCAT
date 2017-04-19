require 'rails_helper'

feature 'User fills out od transform' do
  scenario 'only show input field when log(x+delta) is selected',js:true do
    visit new_assay_path
    choose('assay_transformation_0')
    expect(page).not_to have_selector '#assay_transformation_input'
    
    choose('assay_transformation_01')
    expect(page).not_to have_selector '#assay_transformation_input'
    
    choose('assay_transformation_user')
    expect(page).to have_selector '#assay_transformation_input'
    
    choose('assay_transformation_0')
    expect(page).not_to have_selector '#assay_transformation_input'
  end
end

