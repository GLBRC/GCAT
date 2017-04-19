require 'rails_helper'

feature 'User fills out growth curve model' do
  scenario 'only show input field when loess is selected',js:true do
    visit new_assay_path
    choose('assay_model_sigmoid')
    expect(page).not_to have_selector '#assay_loess_input'
    
    choose('assay_model_loess')
    expect(page).to have_selector '#assay_loess_input'
    
    choose('assay_model_sigmoid')
    expect(page).not_to have_selector '#assay_loess_input'
  end
end

